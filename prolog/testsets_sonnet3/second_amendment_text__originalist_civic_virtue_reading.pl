% ============================================================================
% CONSTRAINT STORY: second_amendment_text__originalist_civic_virtue_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__originalist_civic_virtue_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: second_amendment_text__originalist_civic_virtue_reading
 *   human_readable: Second Amendment as Civic-Republican Militia Guarantee (Originalist Civic Virtue Reading)
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   This story instantiates one of three contested readings of the Second
 *   Amendment's text as a single kernel. Under the originalist civic-virtue
 *   reading, the operative and prefatory clauses are read together as
 *   expressing a unified civic-republican purpose: the constitutional
 *   guarantee exists to preserve a citizenry capable of constituting an armed
 *   militia, drawing on founding-era statutes (e.g. the Militia Act of 1792)
 *   that obligated most free male citizens to own and maintain arms for
 *   common defense. This reading treats 'the people' and 'the militia' as
 *   substantially coextensive, and treats the right as instrumental to a
 *   civic function (collective defense against tyranny, dispersal of martial
 *   capacity away from a professional standing army) rather than as either a
 *   pure individual liberty (the individual_right_reading) or a
 *   state-conditioned regulatory grant (the collective_security_reading). The
 *   ε for this reading is authored low: as a purely interpretive/historical
 *   claim about constitutional meaning, absent enforcement mechanisms of its
 *   own, it does not extract resources from identifiable victims — its 'cost'
 *   is diffuse and interpretive (crowding out competing readings in doctrine
 *   and discourse) rather than material.
 *
 * KEY AGENTS:
 *   - citizenry_as_political_community: diffuse beneficiary of the civic function this reading ascribes to the amendment
 *   - founding_era_militia_tradition_adherents: primary interpretive beneficiaries and reading-generators
 *   - civic_republican_legal_scholars: agenda-setters who construct and refine the reading in doctrine
 *   - contemporary_gun_owners_without_militia_service: excluded from the reading's central rationale despite being commonly assumed beneficiaries under the sibling individual_right_reading
 *   - state_and_federal_firearms_regulators: excluded, since their regulatory interest is subordinated rather than served
 *   - constitutional_historians: analytical observers whose scholarship is contested evidentiary ground for all three readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__originalist_civic_virtue_reading, 0.28).
domain_priors:suppression_score(second_amendment_text__originalist_civic_virtue_reading, 0.22).
domain_priors:theater_ratio(second_amendment_text__originalist_civic_virtue_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__originalist_civic_virtue_reading, rope).
narrative_ontology:human_readable(second_amendment_text__originalist_civic_virtue_reading, "Second Amendment as Civic-Republican Militia Guarantee (Originalist Civic Virtue Reading)").
narrative_ontology:topic_domain(second_amendment_text__originalist_civic_virtue_reading, "constitutional_law/political_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__originalist_civic_virtue_reading, 'aa611bdc-8e7a-4e93-b4d9-92dd327e24c8').
narrative_ontology:cs_kernel_codification('aa611bdc-8e7a-4e93-b4d9-92dd327e24c8', fixed_text).
narrative_ontology:cs_authority_grounding('aa611bdc-8e7a-4e93-b4d9-92dd327e24c8', lineage).
narrative_ontology:cs_interpretation_layer_present('aa611bdc-8e7a-4e93-b4d9-92dd327e24c8').
narrative_ontology:cs_reading_relation('aa611bdc-8e7a-4e93-b4d9-92dd327e24c8', second_amendment_text__collective_security_reading, influences).
narrative_ontology:cs_reading_relation('aa611bdc-8e7a-4e93-b4d9-92dd327e24c8', second_amendment_text__individual_right_reading, coexists_with).
narrative_ontology:cs_axiom('aa611bdc-8e7a-4e93-b4d9-92dd327e24c8', foundational, militia_clause_defines_right_purpose).
narrative_ontology:cs_axiom_status(militia_clause_defines_right_purpose, holdable).
narrative_ontology:cs_axiom_grounding('aa611bdc-8e7a-4e93-b4d9-92dd327e24c8', militia_clause_defines_right_purpose, empirically_contingent).
narrative_ontology:cs_axiom('aa611bdc-8e7a-4e93-b4d9-92dd327e24c8', foundational, civic_obligation_grounds_arms_bearing_not_personal_autonomy).
narrative_ontology:cs_axiom_status(civic_obligation_grounds_arms_bearing_not_personal_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('aa611bdc-8e7a-4e93-b4d9-92dd327e24c8', civic_obligation_grounds_arms_bearing_not_personal_autonomy, conventional).
narrative_ontology:cs_reference_frame('aa611bdc-8e7a-4e93-b4d9-92dd327e24c8', founding_era_universal_militia_obligation).
narrative_ontology:cs_drift_state('aa611bdc-8e7a-4e93-b4d9-92dd327e24c8', post_national_guard_professionalization, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('aa611bdc-8e7a-4e93-b4d9-92dd327e24c8', '').
narrative_ontology:cs_kernel_id(second_amendment_text__originalist_civic_virtue_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__originalist_civic_virtue_reading, citizenry_as_political_community).
narrative_ontology:constraint_beneficiary(second_amendment_text__originalist_civic_virtue_reading, founding_era_militia_tradition_adherents).
narrative_ontology:constraint_beneficiary(second_amendment_text__originalist_civic_virtue_reading, civic_republican_legal_scholars).
narrative_ontology:constraint_vindicates(second_amendment_text__originalist_civic_virtue_reading, civic_republican_theory_of_arms_bearing).
narrative_ontology:constraint_vindicates(second_amendment_text__originalist_civic_virtue_reading, citizen_soldier_ideal).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Understood collectively as the body politic whose members are presumptively armed and available for collective defense against tyranny or invasion. Under this reading, the constitutional guarantee exists to preserve the capacity of ordinary citizens to constitute a militia rather than to rely on a professional standing army. The community as a whole is said to benefit from dispersed civic-military capacity, though no individual member can be pointed to as personally 'collecting' anything measurable.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, citizenry_as_political_community, beneficiary,
    organized, civilizational, constrained, national).

% Scholars, jurists, and advocacy organizations who read the amendment's operative force through the lens of eighteenth-century militia obligation and civic virtue. They author and promote the historical narrative that grounds this reading, cite founding-era militia statutes and republican political theory, and benefit reputationally and institutionally from the reading's persistence in doctrine and public discourse.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, founding_era_militia_tradition_adherents, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_text__originalist_civic_virtue_reading, founding_era_militia_tradition_adherents, agenda_setter).

% Academics and litigators who construct and refine the originalist civic-virtue argument in law review articles, amicus briefs, and judicial opinions. They set the interpretive agenda for this reading specifically, distinguishing it from both the individual-right and collective-security readings, and their professional standing is partly built on the reading's continued relevance.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, civic_republican_legal_scholars, agenda_setter,
    moderate, biographical, mobile, national).

% Individuals who own firearms for personal self-defense or recreation but have no connection to organized militia activity. Under a strict civic-virtue reading their claim to protection is derivative of citizen-soldier status rather than personal autonomy; they are not principal beneficiaries of THIS reading's rationale even though they may benefit incidentally from doctrines that historically grew out of it. Their personal-defense interest is not the reading's stated concern.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, contemporary_gun_owners_without_militia_service, excluded,
    moderate, biographical, constrained, national).

% Legislatures and agencies that would regulate arms possession to serve organized collective defense or public safety. Under this reading their regulatory interest is subordinate to preserving universal citizen access to arms as the substrate of militia capacity; they are not the reading's beneficiaries and their regulatory rationale competes with, rather than is served by, the civic-virtue premise.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, state_and_federal_firearms_regulators, excluded,
    institutional, generational, constrained, national).

% Study founding-era militia statutes, ratification debates, and civic-republican political theory to assess whether the historical record supports universal-citizenry militia understanding as the amendment's operative premise. They take no side in litigation but their scholarship is cited by all three competing readings, including this one, as evidentiary support or challenge.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_text__originalist_civic_virtue_reading, diffuse).
narrative_ontology:fixing_cost_class(second_amendment_text__originalist_civic_virtue_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves a dispersed, non-professionalized capacity for collective self-defense by ensuring the general citizenry remains armed and theoretically available for militia service, avoiding total dependence on a standing army and embedding civic obligation in ordinary citizenship.
% TRANSFER_FUNCTION: The reading does not primarily move material resources between named parties; it moves interpretive authority and legitimacy toward readings and institutions that ground firearms rights in collective civic identity rather than either personal autonomy or state regulatory power, and it moves rhetorical weight toward historical militia tradition in constitutional adjudication.
% ABSENT_VOICES: Contemporary gun owners motivated purely by personal self-defense, and urban communities disproportionately affected by gun violence, are not centered in this reading's rationale — their interests are treated as incidental rather than the amendment's core concern. Firearms regulators seeking collective-safety measures are also structurally absent from this reading's justificatory logic.
% DISAPPEARANCE_RATIONALE: If this specific reading vanished from constitutional discourse, the operative constitutional text would remain and litigation would proceed under the individual-right or collective-security readings instead. Adherents argue the civic-republican understanding is indispensable to correctly bounding the right (e.g., limiting protection to 'lawful, common use' weapons germane to militia service); critics argue its disappearance would simply remove a a historically contestable rationale with little independent doctrinal work left to do post-Heller, since the individual-right reading has substantially displaced it in controlling case law.
% FOUNDING_PROBLEM: Fear of a standing professional army as an instrument of tyranny, combined with reliance on citizen militias for defense and internal order in a society without a large peacetime military establishment.
% FOUNDING_PROBLEM_CORROBORATION: Military historians and constitutional scholars outside the civic-republican advocacy tradition (including some individual-right proponents) attest that the organized state militia system the amendment's text references was effectively superseded by the National Guard system and federal military professionalization in the early twentieth century (Militia Act of 1903 and successors); this corroboration comes from historians and from Supreme Court dicta in Heller itself acknowledging that the militia as historically understood no longer exists in the same form, which is a concession from a competing reading's own textual analysis, not a claim self-asserted by civic-virtue proponents.
narrative_ontology:disappearance_verdict(second_amendment_text__originalist_civic_virtue_reading, contested).
narrative_ontology:founding_problem_status(second_amendment_text__originalist_civic_virtue_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_text__originalist_civic_virtue_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_text__originalist_civic_virtue_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_text__originalist_civic_virtue_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__originalist_civic_virtue_reading_tests).
:- end_tests(second_amendment_text__originalist_civic_virtue_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low (0.28) because, as an interpretive-historical reading rather than an enforcement regime, its direct operation transfers little material resource from identifiable victims to identifiable beneficiaries — its effect is on legitimacy and doctrinal weight, not on money or liberty deprivation. Suppression is modest (0.22): the reading does not by itself coerce compliance; it competes for adoption in courts and public discourse against two live sibling readings. Theater ratio is authored moderate-high and rising then falling (peak ~0.55 around 2008, the Heller decision): as the founding-era militia system became historically defunct while the rhetorical and doctrinal invocation of 'militia' and 'citizen-soldier' language persisted and even intensified in advocacy literature, a growing share of the reading's public deployment became performative invocation of a defunct institutional referent rather than description of live civic-military structures — this is the classic performative-drift signature, though it partially recedes post-Heller as the individual-right reading displaces this one as the operative doctrinal frame. Accessibility collapse is moderate (0.35): historical alternative readings of the amendment's text remain fully articulable and contested; nothing about this reading forecloses access to the historical record. Resistance is moderate-high (0.55): the reading faces active scholarly and judicial contestation, most consequentially from the individual-right reading that has become the controlling doctrine since District of Columbia v. Heller (2008).
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary set here is diffuse and civic rather than a concentrated rent-collecting actor: 'the citizenry as political community' does not collect anything measurable, which is why this reading (unlike the sibling individual_right_reading, which centers a self-defense-motivated gun-owning beneficiary class, or the collective_security_reading, which centers state regulatory authority as an interested actor) authors no victim set at all. The interpretive scholars and advocacy adherents who construct and promote the reading are the closest thing to a concentrated beneficiary, but their benefit is reputational/professional rather than extractive of any other party. This is why the reading computes closer to rope than to any extraction-flavored type: there is coordination function (preserving decentralized civic-military capacity as constitutional value) without an identifiable payer bearing that coordination's cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — fear of standing armies, reliance on organized citizen militias for collective defense — is genealogically dead: the National Guard system and federal military professionalization (culminating in the Militia Act of 1903 and later reforms) substantially replaced the founding-era militia structure this reading takes as its historical referent. Yet the reading persists doctrinally and rhetorically. This is not classic mandatrophy in the extraction sense (no concentrated beneficiary captures rents from an obsolete mandate) — rather it is an interpretive tradition whose founding referent has become historical rather than institutional, and whose contemporary work is increasingly rhetorical (invoking 'citizen-soldier' ideals) rather than descriptive of a live citizen-militia system. The theater_ratio trajectory captures this drift directly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    militia_clause_limiting_or_illustrative,
    'Does the founding-era historical record support reading the prefatory militia clause as substantively limiting the operative right''s scope (this reading''s core premise), or was the clause understood at ratification as merely illustrative of one purpose among several the right served?',
    'Comparative analysis of founding-era state constitutional analogues, ratification debate records, and contemporaneous commentary (e.g. St. George Tucker''s annotations) to determine whether contemporaries treated militia service as a necessary condition of the right or merely its most salient application.',
    'If the clause is genuinely limiting, this reading''s civic-republican account is the historically correct one and the individual_right_reading''s expansion beyond militia-connected purposes is the interpretive departure. If merely illustrative, this reading understates the right''s original scope and the individual_right_reading''s broader construction is better supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_clause_limiting_or_illustrative, empirical, 'Whether founding-era usage supports a limiting or illustrative function for the militia clause.').

omega_variable(
    reading_selection_as_committer_choice,
    'Given that all three sibling readings (collective_security_reading, individual_right_reading, originalist_civic_virtue_reading) draw on overlapping historical evidence, is the selection among them best modeled as a genuine empirical dispute about founding-era meaning, or as a values-driven interpretive choice dressed in originalist historical method?',
    'Track whether adherents of each reading update their historical claims in response to new archival evidence, or whether the historical claims remain stable while only the doctrinal conclusions shift with contemporary policy preference — the latter pattern would indicate the ''historical'' dispute is substantially a proxy for a conceptual/preference dispute.',
    'If empirical, disputes should in principle be resolvable by better historiography. If substantially values-driven, the three readings are better modeled as permanently coexisting positions (as cs_structure.reading_relations declares) rather than as competing empirical hypotheses awaiting resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_as_committer_choice, conceptual, 'Whether the three-way kernel split is an empirical historical dispute or a values dispute wearing historical-method clothing.').

omega_variable(
    diffuse_beneficiary_measurability,
    'Can ''the citizenry as political community'' function as a coherent beneficiary for directionality purposes, or does the absence of any concentrated collecting party make this reading''s beneficiary declaration closer to a vindicated proposition (civic-republican theory) than to a real beneficiary group?',
    'Assess whether any institutional actor (militia advocacy organizations, certain state National Guard constituencies, civic education bodies) can be shown to derive concrete institutional benefit from the reading''s doctrinal persistence, distinct from the purely rhetorical/reputational benefit already captured under founding_era_militia_tradition_adherents.',
    'If no concrete beneficiary exists beyond the interpretive-advocacy class already named, the ''citizenry as political community'' beneficiary entry should be understood as largely aspirational/rhetorical, reinforcing the low extraction and rope-leaning classification. If a concrete institutional beneficiary is identified, the reading would need re-authoring with a more concentrated beneficiary structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diffuse_beneficiary_measurability, conceptual, 'Whether the diffuse civic beneficiary is a real structural beneficiary or an artifact of the reading''s own rhetoric.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__originalist_civic_virtue_reading, 1791, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1791, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 1791, 0.05).
narrative_ontology:measurement(seco_tr_t1900, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 1900, 0.2).
narrative_ontology:measurement(seco_tr_t1939, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 1939, 0.35).
narrative_ontology:measurement(seco_tr_t1980, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 1980, 0.45).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 2008, 0.55).
narrative_ontology:measurement(seco_tr_t2026, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 2026, 0.4).

% Extraction over time
narrative_ontology:measurement(seco_be_t1791, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 1791, 0.05).
narrative_ontology:measurement(seco_be_t1900, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 1900, 0.1).
narrative_ontology:measurement(seco_be_t1939, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 1939, 0.15).
narrative_ontology:measurement(seco_be_t1980, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 1980, 0.2).
narrative_ontology:measurement(seco_be_t2008, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 2008, 0.3).
narrative_ontology:measurement(seco_be_t2026, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 2026, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(second_amendment_text__originalist_civic_virtue_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__originalist_civic_virtue_reading, identity_coordination).
narrative_ontology:affects_constraint(second_amendment_text__originalist_civic_virtue_reading, second_amendment_text__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_text__originalist_civic_virtue_reading, second_amendment_text__collective_security_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the second_amendment_text kernel, each authored as a separate constraint story per the ε-invariance principle: the collective_security_reading (state-conditioned regulatory grant), the individual_right_reading (personal self-defense as core protected activity, controlling doctrine since Heller), and this originalist_civic_virtue_reading (civic-republican citizen-soldier function). All three share the same constitutional text but differ in beneficiary structure, victim structure, and ε: this reading authors the lowest extraction of the three (diffuse civic beneficiary, no victim set), while the individual_right_reading and collective_security_reading each involve more concentrated beneficiary/victim structures tied to contemporary firearms policy stakes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
