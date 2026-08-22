% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_arms_right__individual_right_reading, []).

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
 *   constraint_id: second_amendment_arms_right__individual_right_reading
 *   human_readable: Second Amendment as Individual Pre-Political Liberty (Heller/McDonald Reading)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the individual-right reading of the Second
 *   Amendment kernel — the doctrine articulated in District of Columbia v.
 *   Heller (2008) and McDonald v. City of Chicago (2010) holding that the
 *   right to keep and bear arms belongs to individuals for self-defense,
 *   independent of militia service, and pre-exists government. Prior to 1939
 *   (Miller) and especially prior to 2008, the collective-right and
 *   civic-republican readings were the dominant judicial and scholarly
 *   constructions; the individual-right reading's rise to controlling
 *   doctrine is itself a 20th/21st-century development driven by sustained
 *   constitutional advocacy. This story authors ONLY the individual-right
 *   reading as a clean, self-contained constraint: its own beneficiaries, its
 *   own victims, its own extraction trajectory. The sibling readings
 *   (collective_right_reading, civic_republican_reading) are separate
 *   constraints with their own ε values and are not blended into this one's
 *   classification.
 *
 * KEY AGENTS:
 *   - individual_gun_owners: primary beneficiary (moderate/mobile) — holds the protected liberty
 *   - firearms_industry: organized beneficiary (organized/arbitrage) — commercial stake in doctrine stability
 *   - gun_rights_advocacy_organizations: agenda-setter (organized/arbitrage) — litigates and maintains the doctrine
 *   - municipal_gun_regulators: primary payer (institutional/constrained) — loses regulatory latitude
 *   - communities_with_high_firearm_mortality: powerless payer (powerless/trapped) — bears diffuse cost
 *   - federal_judiciary: agenda-setter/observer (institutional/analytical) — sets doctrinal scope case by case
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__individual_right_reading, 0.58).
domain_priors:suppression_score(second_amendment_arms_right__individual_right_reading, 0.42).
domain_priors:theater_ratio(second_amendment_arms_right__individual_right_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__individual_right_reading, rope).
narrative_ontology:human_readable(second_amendment_arms_right__individual_right_reading, "Second Amendment as Individual Pre-Political Liberty (Heller/McDonald Reading)").
narrative_ontology:topic_domain(second_amendment_arms_right__individual_right_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(second_amendment_arms_right__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__individual_right_reading, 'b2544b2b-6e7d-473d-8d91-b31abd897e44').
narrative_ontology:cs_kernel_codification('b2544b2b-6e7d-473d-8d91-b31abd897e44', fixed_text).
narrative_ontology:cs_authority_grounding('b2544b2b-6e7d-473d-8d91-b31abd897e44', lineage).
narrative_ontology:cs_interpretation_layer_present('b2544b2b-6e7d-473d-8d91-b31abd897e44').
narrative_ontology:cs_reading_relation('b2544b2b-6e7d-473d-8d91-b31abd897e44', second_amendment_arms_right__collective_right_reading, forecloses).
narrative_ontology:cs_reading_relation('b2544b2b-6e7d-473d-8d91-b31abd897e44', second_amendment_arms_right__civic_republican_reading, coexists_with).
narrative_ontology:cs_axiom('b2544b2b-6e7d-473d-8d91-b31abd897e44', foundational, arms_right_is_individual_and_pre_political).
narrative_ontology:cs_axiom_status(arms_right_is_individual_and_pre_political, holdable).
narrative_ontology:cs_axiom_grounding('b2544b2b-6e7d-473d-8d91-b31abd897e44', arms_right_is_individual_and_pre_political, deontological).
narrative_ontology:cs_axiom('b2544b2b-6e7d-473d-8d91-b31abd897e44', foundational, militia_clause_is_prefatory_not_operative).
narrative_ontology:cs_axiom_status(militia_clause_is_prefatory_not_operative, holdable).
narrative_ontology:cs_axiom_grounding('b2544b2b-6e7d-473d-8d91-b31abd897e44', militia_clause_is_prefatory_not_operative, conventional).
narrative_ontology:cs_reference_frame('b2544b2b-6e7d-473d-8d91-b31abd897e44', founding_era_natural_rights_liberty).
narrative_ontology:cs_drift_state('b2544b2b-6e7d-473d-8d91-b31abd897e44', post_heller_mcdonald_bruen_era, gap(revival_pressure, severe, true)).
narrative_ontology:cs_created_at('b2544b2b-6e7d-473d-8d91-b31abd897e44', '').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__individual_right_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__individual_right_reading, individual_gun_owners).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__individual_right_reading, firearms_industry).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__individual_right_reading, gun_rights_advocacy_organizations).
narrative_ontology:constraint_victim(second_amendment_arms_right__individual_right_reading, municipal_gun_regulators).
narrative_ontology:constraint_victim(second_amendment_arms_right__individual_right_reading, gun_violence_prevention_advocates).
narrative_ontology:constraint_victim(second_amendment_arms_right__individual_right_reading, communities_with_high_firearm_mortality).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__individual_right_reading, natural_rights_pre_political_liberty_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__individual_right_reading, textualist_originalist_interpretive_method).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold a constitutionally protected entitlement to keep and bear arms for self-defense, independent of militia service. Can acquire, keep, and in most jurisdictions carry firearms without needing to justify the choice to the state as connected to organized defense. Exit from the constraint's protection is not sought — they are its intended beneficiaries — though they can relocate to jurisdictions with looser regulation if a home state pushes restrictions to the edge of what the doctrine tolerates.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, individual_gun_owners, beneficiary,
    moderate, biographical, mobile, national).

% Manufactures and sells firearms into a market whose size and legal security depend directly on the individual-right reading remaining doctrine. Funds litigation and lobbying to defend and extend the reading; benefits from every judicial reaffirmation that narrows the space for regulation. Can shift production and marketing across state lines to exploit variance in how aggressively the doctrine is enforced against local restrictions.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, firearms_industry, beneficiary,
    organized, generational, arbitrage, national).

% Litigate test cases, draft model legislation, and coordinate the originalist scholarship that gives the individual-right reading its doctrinal architecture. Function as the reading's active maintainers — courts do not sustain a constitutional interpretation on their own; these organizations supply the sustained advocacy, funding, and case selection that keep the reading live and expanding.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, gun_rights_advocacy_organizations, agenda_setter,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_arms_right__individual_right_reading, gun_rights_advocacy_organizations, beneficiary).

% City and state governments that seek to regulate firearm possession, carry, or types in response to local violence patterns must now design regulation to survive strict or heightened scrutiny under the individual-right doctrine. Legislation once treated as routine police power is struck down or chilled before enactment. Cannot exit the constraint's reach — it binds them as sub-federal government regardless of local preference, per incorporation against the states.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, municipal_gun_regulators, payer,
    institutional, generational, constrained, regional).

% Organize around policy responses to firearm injury and death — waiting periods, assault-weapon restrictions, permit requirements, red-flag laws — and find the menu of achievable policy narrowed by the doctrine's expanding scope. Cannot exit the constitutional terrain; every proposal must be built to survive the individual-right framework or risk swift invalidation.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, gun_violence_prevention_advocates, payer,
    moderate, biographical, constrained, national).

% Bear the disproportionate burden of firearm injury and death in neighborhoods where the doctrine's expansive protection intersects with weak enforcement capacity and high gun density. Have essentially no exit — cannot relocate en masse, cannot alter the constitutional doctrine through local political action, and depend on regulatory tools the doctrine increasingly forecloses.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, communities_with_high_firearm_mortality, payer,
    powerless, biographical, trapped, local).

% Adjudicates the boundary of the individual right in each new case, applying originalist historical-analogue methodology (post-Bruen) to determine which regulations survive. Functions simultaneously as analytical arbiter and as the active mechanism that sets and re-sets the doctrine's practical scope with each ruling.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, federal_judiciary, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_arms_right__individual_right_reading, federal_judiciary, agenda_setter).

% Scholars and advocates who hold that the constitutional text protects militia-connected or civic-republican arms-bearing rather than a free-standing individual liberty. Their reading was the dominant academic and judicial position for most of the 20th century but was displaced by Heller/McDonald; they continue to publish and litigate but operate outside the doctrine that now governs actual case outcomes.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, collective_and_civic_republican_theorists, excluded,
    moderate, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_arms_right__individual_right_reading, diffuse).
narrative_ontology:fixing_cost_class(second_amendment_arms_right__individual_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides individuals a stable, judicially enforceable expectation that firearm possession for self-defense will not be criminalized by federal or (after incorporation) state and local government, coordinating expectations around a fixed baseline of personal liberty rather than leaving the scope of the right to shifting legislative majorities.
% TRANSFER_FUNCTION: Moves regulatory authority away from municipal and state legislatures and toward constitutional doctrine and the federal judiciary; moves policy risk and the costs of firearm proliferation toward communities and advocates seeking regulation, while moving legal security and market certainty toward gun owners and the firearms industry.
% ABSENT_VOICES: Gun violence prevention advocates and residents of high-mortality communities are present in the political process but structurally out-voted in the constitutional forum — the doctrine is set by courts applying historical-analogue methodology in which contemporary public-health evidence and lived harm carry no doctrinal weight. Civic-republican and collective-right scholars had their reading judicially foreclosed as case-controlling doctrine in Heller (2008) and are now voices outside the binding framework.
% DISAPPEARANCE_RATIONALE: If the individual-right reading were overturned overnight, municipal and state legislatures would regain the latitude to enact firearm restrictions (waiting periods, assault-weapon bans, carry limits) without surviving heightened constitutional scrutiny; the firearms industry would face materially higher regulatory risk and litigation exposure; gun rights organizations would lose their principal doctrinal lever; and gun violence prevention advocates would regain policy tools currently foreclosed or chilled.
% FOUNDING_PROBLEM: The historical concern (as this reading construes it) was to preserve, against a newly formed federal government, the pre-existing natural right of individuals to possess arms for self-defense and to resist tyranny — a right the framers understood as already held by the people, not granted by the militia clause or by government.
% FOUNDING_PROBLEM_CORROBORATION: Originalist legal scholars and gun rights organizations attest the founding problem (federal disarmament of the individual) remains structurally live given ongoing legislative attempts at firearm restriction. Legal historians outside the gun-rights coalition and the pre-Heller judicial and academic consensus (which for most of the 20th century read the Amendment as militia-connected) attest that the individual-right construction is itself a late-20th-century reconstruction of the founding problem, not a continuous doctrinal thread — corroboration for the individual-right founding narrative comes substantially from within the advocacy and originalist scholarly ecosystem that benefits from it, with prominent dissent from historians who do not share that stake.
narrative_ontology:disappearance_verdict(second_amendment_arms_right__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_arms_right__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__individual_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_arms_right__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_arms_right__individual_right_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_arms_right__individual_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_arms_right__individual_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_arms_right__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored as moderate-high (0.58) and rising sharply after 2008: pre-Heller, the individual-right construction had limited practical bite because it was not controlling doctrine, so its extractive effect on regulatory authority was low. After Heller/McDonald and especially after Bruen's historical-analogue test, the doctrine actively displaces municipal and state police-power regulation, so extraction (measured as regulatory authority and public-safety policy space transferred away from communities and regulators toward the protected individual liberty and its beneficiaries) rises. Suppression is authored lower than extraction (0.42) because the mechanism operates primarily through judicial invalidation of statutes rather than through direct coercive enforcement against dissenting jurisdictions — though the suppression trajectory rises in step with extraction as the historical-analogue methodology forecloses a widening range of regulatory alternatives. Theater ratio is modest (0.28): the doctrine performs genuine constitutional adjudication, not mere ritual, though originalist historical argument sometimes serves as post-hoc justification for outcomes reached on other grounds.
 *
 * PERSPECTIVAL GAP:
 *   From the individual gun owner's seat, this is a rope: a stable, hard-won guarantee against arbitrary disarmament, with minimal coercive overhead borne by the beneficiary and no exit needed because the constraint's whole function is to protect their position. From the municipal regulator's or high-mortality-community's seat, the same doctrine looks tangled-rope-to-snare: a genuine liberty-coordination function for owners, layered with an asymmetric transfer of regulatory capacity and public-safety cost onto communities that cannot opt out. The engine computes each seat's type from the structural data; the divergence between the beneficiary's rope-reading and the payer's more extractive reading is the substantive finding, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual gun owners and the firearms industry are declared beneficiaries because the doctrine directly expands their protected liberty and market security — low d, damped extraction from their seat. Municipal regulators, prevention advocates, and high-mortality communities are declared victims because the doctrine's expansion directly narrows their available policy tools and shifts cost onto them — high d, amplified extraction from their seat. Gun rights advocacy organizations sit as both agenda-setter and beneficiary (secondary_role) because they actively construct and defend the doctrine rather than passively receiving it. The federal judiciary sits as observer/agenda-setter because it exercises genuine interpretive discretion in each case while also being the mechanism through which the doctrine's practical scope is set.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding-problem interview surfaces a genealogy gap characteristic of doctrines whose founding narrative is substantially self-corroborated: the individual-right reading's proponents describe a continuous founding-era liberty under contemporary threat, while historians outside the advocacy coalition describe the individual-right construction as a late-20th-century reconstruction that displaced a decades-long collective-right judicial consensus. This does not by itself indicate mandatrophy (a fixed constitutional text with an active, contested interpretive layer is a standard commitment-system shape, not a lapsed mandate) — but the contested corroboration is exactly the kind of divergence the six-questions genealogy interview is built to surface rather than adjudicate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the individual-right reading the historically correct construction of the Second Amendment''s original public meaning, or a late-20th-century doctrinal reconstruction displacing a longer-standing collective-right/civic-republican consensus?',
    'Historical linguistic and legislative-record analysis of founding-era usage of ''keep and bear arms'' and ''militia,'' cross-checked against the trajectory of 19th- and 20th-century judicial and scholarly treatment prior to Heller (2008).',
    'If the individual-right reading is a reconstruction rather than continuous doctrine, the founding_problem narrative authored here is itself substantially a product of the advocacy movement that benefits from it, which would deepen (not resolve) the mandatrophy-adjacent genealogy concern documented in commentary.mandatrophy_analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether this reading recovers or reconstructs the kernel''s original meaning.').

omega_variable(
    sibling_reading_structural_delta,
    'How much of this reading''s authored extraction (0.58) is attributable specifically to the individual-right construction versus to firearms policy dynamics that would persist under either sibling reading (collective_right or civic_republican)?',
    'Comparative doctrinal analysis: model regulatory outcomes for representative firearm statutes (assault weapon bans, waiting periods, carry restrictions) under each of the three kernel readings and measure the divergence in which statutes survive judicial review.',
    'A finding that regulatory outcomes diverge sharply across readings would confirm the readings are structurally distinct constraints (as authored, per the ε-invariance decomposition); a finding of substantial outcome convergence would suggest the kernel readings matter less in practice than doctrinal rhetoric suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, empirical, 'How much of the measured extraction is specifically attributable to this reading versus shared policy dynamics.').

omega_variable(
    natural_right_vs_constructed_liberty,
    'Is the ''pre-existing, pre-political'' character of the right (as this reading asserts) a genuine natural-law claim independent of government recognition, or is ''pre-existing'' itself a rhetorical device that naturalizes what is actually a contested, judicially constructed and actively defended doctrinal position?',
    'Philosophical and historical analysis of natural-rights theory as understood by the framers, compared against the doctrine''s actual dependence on sustained litigation, advocacy funding, and shifting judicial composition for its practical scope.',
    'If the right''s practical scope depends substantially on contingent judicial composition and sustained advocacy rather than self-executing natural law, the individual-right reading functions less like a mountain (irreducible natural limit) and more like an actively maintained doctrinal construction with real beneficiaries — relevant to why this story is authored as rope/tangled-rope-adjacent rather than mountain despite the reading''s own natural-rights framing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_right_vs_constructed_liberty, conceptual, 'Whether the reading''s pre-political framing is a substantive natural-law claim or a naturalizing rhetorical move.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__individual_right_reading, 1791, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1791, second_amendment_arms_right__individual_right_reading, theater_ratio, 1791, 0.1).
narrative_ontology:measurement(seco_tr_t1900, second_amendment_arms_right__individual_right_reading, theater_ratio, 1900, 0.12).
narrative_ontology:measurement(seco_tr_t1939, second_amendment_arms_right__individual_right_reading, theater_ratio, 1939, 0.15).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_arms_right__individual_right_reading, theater_ratio, 2008, 0.2).
narrative_ontology:measurement(seco_tr_t2010, second_amendment_arms_right__individual_right_reading, theater_ratio, 2010, 0.22).
narrative_ontology:measurement(seco_tr_t2022, second_amendment_arms_right__individual_right_reading, theater_ratio, 2022, 0.26).
narrative_ontology:measurement(seco_tr_t2024, second_amendment_arms_right__individual_right_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(seco_be_t1791, second_amendment_arms_right__individual_right_reading, base_extractiveness, 1791, 0.15).
narrative_ontology:measurement(seco_be_t1900, second_amendment_arms_right__individual_right_reading, base_extractiveness, 1900, 0.2).
narrative_ontology:measurement(seco_be_t1939, second_amendment_arms_right__individual_right_reading, base_extractiveness, 1939, 0.22).
narrative_ontology:measurement(seco_be_t2008, second_amendment_arms_right__individual_right_reading, base_extractiveness, 2008, 0.45).
narrative_ontology:measurement(seco_be_t2010, second_amendment_arms_right__individual_right_reading, base_extractiveness, 2010, 0.5).
narrative_ontology:measurement(seco_be_t2022, second_amendment_arms_right__individual_right_reading, base_extractiveness, 2022, 0.56).
narrative_ontology:measurement(seco_be_t2024, second_amendment_arms_right__individual_right_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1791, second_amendment_arms_right__individual_right_reading, suppression_requirement, 1791, 0.1).
narrative_ontology:measurement(seco_su_t1900, second_amendment_arms_right__individual_right_reading, suppression_requirement, 1900, 0.12).
narrative_ontology:measurement(seco_su_t1939, second_amendment_arms_right__individual_right_reading, suppression_requirement, 1939, 0.15).
narrative_ontology:measurement(seco_su_t2008, second_amendment_arms_right__individual_right_reading, suppression_requirement, 2008, 0.32).
narrative_ontology:measurement(seco_su_t2010, second_amendment_arms_right__individual_right_reading, suppression_requirement, 2010, 0.36).
narrative_ontology:measurement(seco_su_t2022, second_amendment_arms_right__individual_right_reading, suppression_requirement, 2022, 0.4).
narrative_ontology:measurement(seco_su_t2024, second_amendment_arms_right__individual_right_reading, suppression_requirement, 2024, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__individual_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_arms_right__individual_right_reading, 0.1).
narrative_ontology:affects_constraint(second_amendment_arms_right__individual_right_reading, collective_right_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__individual_right_reading, civic_republican_reading).

% DUAL FORMULATION NOTE:
% Part of the second_amendment_arms_right kernel family (3 readings). This story (individual_right_reading) authors high ε on prohibition/regulation measures and places individual gun owners in the beneficiary set and regulatory authorities in the victim set — a structurally distinct constraint from collective_right_reading (which would place organized militia authority as beneficiary and individual ownership claims as unprotected) and civic_republican_reading (which centers armed citizenship as a civic prerequisite, with different beneficiary/victim mapping again). Each reading is authored as its own constraint with its own stable ε; do not average across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
