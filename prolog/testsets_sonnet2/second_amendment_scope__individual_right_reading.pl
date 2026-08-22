% ============================================================================
% CONSTRAINT STORY: second_amendment_scope__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_scope__individual_right_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: second_amendment_scope__individual_right_reading
 *   human_readable: Second Amendment as Individual Right Unconnected to Militia Service (Heller/McDonald Reading)
 *   domain: constitutional_law/political_theory/rights_jurisprudence
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested Second Amendment
 *   kernel: the individual-right reading, under which the Amendment protects
 *   a personal right to keep and bear arms unconnected to militia service —
 *   the reading adopted by the U.S. Supreme Court in District of Columbia v.
 *   Heller (2008) and incorporated against the states in McDonald v. Chicago
 *   (2010), and extended in New York State Rifle & Pistol Association v.
 *   Bruen (2022). Prior to 2008, this reading existed primarily in
 *   scholarship and dissent; since 2008 it is the controlling doctrinal
 *   framework. The story evaluates ONLY this reading's structure — its own
 *   beneficiary set, its own extraction profile, its own persistence dynamics
 *   — not the kernel contest as a whole. Sibling readings
 *   (collective_right_reading, civic_right_reading) are separate constraint
 *   stories with their own ε values; they are not blended here.
 *
 * KEY AGENTS:
 *   - individual_gun_owners: primary beneficiary (moderate/mobile) — right recognized and protected
 *   - firearms_industry: structural beneficiary (organized/arbitrage) — market protected from regulatory contraction
 *   - gun_rights_advocacy_organizations: agenda-setter (organized/arbitrage) — litigates and lobbies to establish and extend the reading
 *   - gun_violence_victims_and_survivors: primary payer (powerless/trapped) — bears externalized risk
 *   - municipal_governments_seeking_regulation: institutional payer (institutional/constrained) — loses policy latitude
 *   - communities_with_high_gun_mortality: concentrated payer (powerless/trapped) — bears disproportionate local cost
 *   - state_and_federal_courts: agenda-setter/observer (institutional/analytical) — constitutes and enforces doctrinal boundaries
 *   - collective_and_civic_right_advocates: excluded (organized/constrained) — doctrinally foreclosed sibling reading, though academically live
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__individual_right_reading, 0.62).
domain_priors:suppression_score(second_amendment_scope__individual_right_reading, 0.58).
domain_priors:theater_ratio(second_amendment_scope__individual_right_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_scope__individual_right_reading, "Second Amendment as Individual Right Unconnected to Militia Service (Heller/McDonald Reading)").
narrative_ontology:topic_domain(second_amendment_scope__individual_right_reading, "constitutional_law/political_theory/rights_jurisprudence").

domain_priors:requires_active_enforcement(second_amendment_scope__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__individual_right_reading, 'e13d7a15-87cb-4bf9-a719-6d846c8b8e96').
narrative_ontology:cs_kernel_codification('e13d7a15-87cb-4bf9-a719-6d846c8b8e96', fixed_text).
narrative_ontology:cs_authority_grounding('e13d7a15-87cb-4bf9-a719-6d846c8b8e96', lineage).
narrative_ontology:cs_interpretation_layer_present('e13d7a15-87cb-4bf9-a719-6d846c8b8e96').
narrative_ontology:cs_reading_relation('e13d7a15-87cb-4bf9-a719-6d846c8b8e96', second_amendment_scope__collective_right_reading, forecloses).
narrative_ontology:cs_reading_relation('e13d7a15-87cb-4bf9-a719-6d846c8b8e96', second_amendment_scope__civic_right_reading, coexists_with).
narrative_ontology:cs_axiom('e13d7a15-87cb-4bf9-a719-6d846c8b8e96', foundational, individual_natural_right_predates_militia_clause).
narrative_ontology:cs_axiom_status(individual_natural_right_predates_militia_clause, holdable).
narrative_ontology:cs_axiom_grounding('e13d7a15-87cb-4bf9-a719-6d846c8b8e96', individual_natural_right_predates_militia_clause, deontological).
narrative_ontology:cs_axiom('e13d7a15-87cb-4bf9-a719-6d846c8b8e96', foundational, prefatory_clause_announces_purpose_but_does_not_limit_operative_clause).
narrative_ontology:cs_axiom_status(prefatory_clause_announces_purpose_but_does_not_limit_operative_clause, holdable).
narrative_ontology:cs_axiom_grounding('e13d7a15-87cb-4bf9-a719-6d846c8b8e96', prefatory_clause_announces_purpose_but_does_not_limit_operative_clause, conventional).
narrative_ontology:cs_reference_frame('e13d7a15-87cb-4bf9-a719-6d846c8b8e96', founding_era_individual_natural_right).
narrative_ontology:cs_drift_state('e13d7a15-87cb-4bf9-a719-6d846c8b8e96', post_heller_doctrinal_consolidation, gap(revival_pressure, severe, true)).
narrative_ontology:cs_created_at('e13d7a15-87cb-4bf9-a719-6d846c8b8e96', '').
narrative_ontology:cs_kernel_id(second_amendment_scope__individual_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, individual_gun_owners).
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, firearms_industry).
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, gun_rights_advocacy_organizations).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, gun_violence_victims_and_survivors).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, municipal_governments_seeking_regulation).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, communities_with_high_gun_mortality).
narrative_ontology:constraint_vindicates(second_amendment_scope__individual_right_reading, individual_natural_right_to_self_defense).
narrative_ontology:constraint_vindicates(second_amendment_scope__individual_right_reading, originalist_textual_methodology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own or wish to own firearms for self-defense, hunting, or recreation without militia affiliation. Under this reading, their right is constitutionally protected against most state and local restriction; they can acquire, keep, and in many jurisdictions carry firearms with fewer legal barriers than under a militia-conditioned reading.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, individual_gun_owners, beneficiary,
    moderate, biographical, mobile, national).

% Manufactures and sells firearms and ammunition to a mass individual consumer market that this reading legally guarantees against many regulatory restrictions. Funds litigation and lobbying that entrenches the individual-right interpretation, benefiting directly from market size the interpretation protects.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, firearms_industry, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_scope__individual_right_reading, firearms_industry, agenda_setter).

% Litigate, lobby, and mobilize voters to establish and defend the individual-right reading in courts and legislatures. Set litigation strategy, select test cases, and shape the doctrinal boundaries (what counts as a permissible regulation) that flow from the reading's adoption.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, gun_rights_advocacy_organizations, agenda_setter,
    organized, generational, arbitrage, national).

% Bear the downstream costs of expanded firearms availability and constrained regulatory response — homicide, suicide, and injury risk elevated by circulation this reading protects from many restriction efforts. Have no direct standing to alter the constitutional interpretation; their harms are externalities of a right they do not hold in any comparable enforceable form.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, gun_violence_victims_and_survivors, payer,
    powerless, biographical, trapped, national).

% Wish to enact local firearms regulation (assault weapon bans, carry restrictions, storage requirements) in response to local violence patterns but face strict-scrutiny-style judicial review under this reading that strikes down or chills legislation. Their policy tools are narrowed regardless of local democratic mandate.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, municipal_governments_seeking_regulation, payer,
    institutional, generational, constrained, regional).

% Disproportionately urban and often low-income communities bearing concentrated gun violence; under this reading, local regulatory experimentation tailored to their conditions is constitutionally constrained by a national doctrinal ceiling set largely by litigation elsewhere. Cannot easily relocate away from the risk and have limited capacity to litigate the interpretation itself.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, communities_with_high_gun_mortality, payer,
    powerless, generational, trapped, local).

% Adjudicate the scope of the individual right, define permissible regulatory categories (e.g., felon dispossession, sensitive places), and enforce the doctrinal framework through case law. Their interpretive choices actively constitute and maintain the reading's practical boundaries.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, state_and_federal_courts, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_scope__individual_right_reading, state_and_federal_courts, observer).

% Hold that the Amendment's prefatory militia clause meaningfully limits or conditions the right, or vests it in states rather than individuals unconnected to service. Since Heller (2008) and McDonald (2010), their reading has been doctrinally foreclosed as controlling precedent even though it remains actively argued in scholarship, dissenting opinions, and some state constitutional contexts.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, collective_and_civic_right_advocates, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_scope__individual_right_reading, firearms_industry).
narrative_ontology:fixing_cost_class(second_amendment_scope__individual_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, nationally uniform baseline defining what counts as protected firearms ownership, allowing individuals, manufacturers, and courts to coordinate expectations about lawful possession without case-by-case relitigation of the right's basic existence.
% TRANSFER_FUNCTION: Shifts regulatory authority away from municipal and state legislatures (who previously could restrict firearms more freely under a militia-conditioned or collective reading) toward individual owners and the firearms industry, and shifts risk exposure from the industry/owner side toward communities and individuals affected by resulting gun availability.
% ABSENT_VOICES: Gun violence victims, their families, and residents of high-mortality communities have no comparable constitutional voice in the doctrine's formation; their harms register as policy externalities rather than rights claims. Collective/civic-right scholars and dissenting jurists remain in the conversation but their reading has lost controlling doctrinal force.
% DISAPPEARANCE_RATIONALE: If the individual-right reading were overturned overnight in favor of a collective or civic reading, thousands of state and municipal firearms regulations currently struck down or chilled under strict-scrutiny-style review would become newly viable; litigation strategy built around Heller/McDonald/Bruen would collapse; the firearms industry's constitutional shield against regulation would substantially narrow.
% FOUNDING_PROBLEM: The Second Amendment's ratification-era text was read (in this reading) to secure a pre-existing individual natural right to keep and bear arms for self-defense, not merely to preserve state militia capacity against federal disarmament.
% FOUNDING_PROBLEM_CORROBORATION: The reading is corroborated within originalist legal scholarship and by the Heller/McDonald/Bruen majority opinions themselves. Historians outside the advocacy tradition (some originalist, some not) dispute whether founding-era practice and the 1791 debates support an individual right unconnected to militia service as opposed to a civic-republican or state-preservation reading; this dispute is unresolved in the historical record and is attested by scholars on multiple sides, not solely by parties who benefit from either reading.
narrative_ontology:disappearance_verdict(second_amendment_scope__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_scope__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__individual_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_scope__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_scope__individual_right_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_scope__individual_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_scope__individual_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_scope__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate-high (0.62) reflecting the expected structural delta: this reading's broad individual coverage plus strict-scrutiny-style judicial review under Bruen substantially constrains state and municipal regulatory authority nationwide, shifting risk toward those harmed by firearms availability without a comparable enforceable countervailing right on their side. Suppression is authored moderate-high (0.58) because since 2008 the doctrinal apparatus actively strikes down or chills a wide range of local and state regulation — this is coercive judicial enforcement of one reading against contrary legislative preferences, not passive coexistence. Theater ratio is comparatively low (0.28): the doctrine's operative content (case-by-case Second Amendment litigation, actual injunctions against specific statutes) is substantively enforced rather than merely performed, though some 'sensitive places' and 'text, history, and tradition' analysis has an increasingly formulaic, precedent-shopping character that inflates the ratio somewhat over time. Accessibility collapse is moderate (0.5): alternative readings remain articulable in scholarship and dissent, but practical policy alternatives at the municipal level have substantially narrowed since Heller/Bruen. Resistance is high (0.72): the reading faces sustained, organized opposition from public health researchers, victim advocacy groups, and a substantial share of state and local legislatures attempting workarounds.
 *
 * PERSPECTIVAL GAP:
 *   Individual gun owners and the firearms industry experience this constraint as a genuine coordination good — a stable, judicially guaranteed baseline enabling lawful commerce and personal autonomy. Municipal governments and high-mortality communities experience the same doctrinal structure as an externally imposed extraction of their policy capacity, with costs measured in constrained local response to documented violence patterns. The engine should compute divergent seat-level types from this same structural data: something closer to a rope from the beneficiary seats, something closer to a snare or tangled_rope from the payer seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual gun owners and the firearms industry are declared beneficiaries: the reading directly expands their protected activity and market, pushing their directionality toward the beneficiary end. Gun violence victims, municipal governments, and high-mortality communities are declared victims: they bear diffuse or concentrated costs of the reading's regulatory-constraining effect without a comparable enforceable right of their own, pushing directionality toward the target end. Courts occupy an agenda-setting/observer role — they constitute the doctrine but do not personally collect or pay in the market sense; their exit option is coded analytical because their relationship to the constraint is adjudicative rather than economic.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (securing a pre-existing individual natural right against disarmament) is authored as status=contested rather than clearly live or dead, because reasonable historical and doctrinal disagreement persists about whether the original public meaning supports an individual reading unconnected to militia service at all. This prevents the classification from either (a) treating the reading as settled natural law (mountain) when it remains actively contested constitutional interpretation, or (b) dismissing it as pure extraction when it does perform a real coordination function (stable expectations for lawful ownership) for a very large beneficiary class. The tangled_rope classification captures both: genuine coordination for individual owners and industry, layered with asymmetric cost-shifting onto municipal regulators and violence-affected communities, sustained by active judicial enforcement (strict-scrutiny-style review) rather than voluntary consensus.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    originalist_history_indeterminacy,
    'Does the founding-era historical record (ratification debates, contemporaneous commentary, early state constitutional analogues) actually support an individual right unconnected to militia service, or does it support a civic-republican or state-preservation reading instead?',
    'Continued historical scholarship examining founding-era militia statutes, personal firearms regulation in the colonies and early republic, and the drafting history of the Second Amendment''s prefatory and operative clauses; resolution is unlikely to be definitive given the genre of historical argument involved, but the weight of evidence could shift.',
    'If the historical record substantially favors the collective or civic reading, the individual-right reading''s originalist legitimacy claim weakens considerably, supporting a re-reading of Heller/Bruen as constructed doctrine rather than restored original meaning — this would not change this story''s own ε but would affect how confidently the reading''s foundational axiom can be held.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(originalist_history_indeterminacy, empirical, 'Whether founding-era history supports the individual-right reading against its siblings.').

omega_variable(
    reading_selection_and_judicial_composition,
    'Is the individual-right reading''s current doctrinal dominance a function of correct constitutional interpretation, or a function of the composition of the Supreme Court at the moment Heller, McDonald, and Bruen were decided?',
    'Comparative analysis of pre-2008 circuit court consensus (which largely favored the collective-right reading) against post-2008 doctrine, cross-referenced with changes in Court composition and confirmation politics over the same period.',
    'If doctrinal dominance tracks judicial composition rather than convergent legal reasoning, the individual-right reading''s persistence is better modeled as contingent political capture of interpretive authority than as settled constitutional meaning — this bears on how the story''s authority_grounding and drift_state should be read going forward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_and_judicial_composition, conceptual, 'Whether the reading''s dominance reflects legal reasoning or judicial composition contingency.').

omega_variable(
    beneficiary_coupling_with_industry_lobbying,
    'To what extent has the firearms industry''s litigation funding and lobbying shaped which test cases reached the Supreme Court and how the individual-right reading''s boundaries have been drawn, versus the reading emerging independently from judicial reasoning?',
    'Tracing amicus brief funding, litigation sponsorship, and case selection strategy by advocacy organizations across the Heller-McDonald-Bruen line of cases.',
    'High coupling would support treating the industry beneficiary relationship as an active driver of doctrinal content (consistent with tangled_rope''s enforcement-plus-extraction structure) rather than an incidental byproduct of a reading arrived at independently.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_coupling_with_industry_lobbying, empirical, 'Whether industry funding shaped doctrinal content and case selection.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__individual_right_reading, 1791, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1791, second_amendment_scope__individual_right_reading, theater_ratio, 1791, 0.1).
narrative_ontology:measurement(seco_tr_t1900, second_amendment_scope__individual_right_reading, theater_ratio, 1900, 0.12).
narrative_ontology:measurement(seco_tr_t1960, second_amendment_scope__individual_right_reading, theater_ratio, 1960, 0.15).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_scope__individual_right_reading, theater_ratio, 2008, 0.2).
narrative_ontology:measurement(seco_tr_t2010, second_amendment_scope__individual_right_reading, theater_ratio, 2010, 0.22).
narrative_ontology:measurement(seco_tr_t2022, second_amendment_scope__individual_right_reading, theater_ratio, 2022, 0.26).
narrative_ontology:measurement(seco_tr_t2026, second_amendment_scope__individual_right_reading, theater_ratio, 2026, 0.28).

% Extraction over time
narrative_ontology:measurement(seco_be_t1791, second_amendment_scope__individual_right_reading, base_extractiveness, 1791, 0.2).
narrative_ontology:measurement(seco_be_t1900, second_amendment_scope__individual_right_reading, base_extractiveness, 1900, 0.22).
narrative_ontology:measurement(seco_be_t1960, second_amendment_scope__individual_right_reading, base_extractiveness, 1960, 0.25).
narrative_ontology:measurement(seco_be_t2008, second_amendment_scope__individual_right_reading, base_extractiveness, 2008, 0.5).
narrative_ontology:measurement(seco_be_t2010, second_amendment_scope__individual_right_reading, base_extractiveness, 2010, 0.54).
narrative_ontology:measurement(seco_be_t2022, second_amendment_scope__individual_right_reading, base_extractiveness, 2022, 0.6).
narrative_ontology:measurement(seco_be_t2026, second_amendment_scope__individual_right_reading, base_extractiveness, 2026, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1791, second_amendment_scope__individual_right_reading, suppression_requirement, 1791, 0.1).
narrative_ontology:measurement(seco_su_t1900, second_amendment_scope__individual_right_reading, suppression_requirement, 1900, 0.15).
narrative_ontology:measurement(seco_su_t1960, second_amendment_scope__individual_right_reading, suppression_requirement, 1960, 0.2).
narrative_ontology:measurement(seco_su_t2008, second_amendment_scope__individual_right_reading, suppression_requirement, 2008, 0.45).
narrative_ontology:measurement(seco_su_t2010, second_amendment_scope__individual_right_reading, suppression_requirement, 2010, 0.5).
narrative_ontology:measurement(seco_su_t2022, second_amendment_scope__individual_right_reading, suppression_requirement, 2022, 0.55).
narrative_ontology:measurement(seco_su_t2026, second_amendment_scope__individual_right_reading, suppression_requirement, 2026, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_scope__individual_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_scope__individual_right_reading, 0.1).
narrative_ontology:affects_constraint(second_amendment_scope__individual_right_reading, collective_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__individual_right_reading, civic_right_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings decomposed from the natural-language 'Second Amendment scope' kernel per the ε-invariance principle. collective_right_reading (state militia authority, minimal individual beneficiary set, low ε) and civic_right_reading (individual right conditioned on militia participation, intermediate beneficiary set, moderate ε) are separate stories. This story (individual_right_reading) carries the highest ε of the three because its beneficiary set is broadest (all individuals) and its constraint on state regulatory authority is most severe (strict-scrutiny-style review under Bruen). Do not average these three ε values — each reading is evaluated independently by its own lights, per the fixed ε-referent rule for kernel readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
