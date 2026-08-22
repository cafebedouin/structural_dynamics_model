% ============================================================================
% CONSTRAINT STORY: dharmasastra_corpus__orthodox_literalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dharmasastra_corpus__orthodox_literalist, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: dharmasastra_corpus__orthodox_literalist
 *   human_readable: Orthodox Literalist Reading of Dharmasastra Varna/Jati Hierarchy as Eternal Revealed Law
 *   domain: religious_law/textual_interpretation/normative_authority
 *
 * SUMMARY:
 *   This story authors the orthodox literalist reading of the dharmasastra
 *   kernel: the position that varna/jati hierarchy prescriptions in texts
 *   like Manusmriti are eternal (sanatana) revealed truth (shruti-derived or
 *   shruti-continuous smriti) requiring literal, unmediated observance rather
 *   than historical or ethical reinterpretation. This is one of three
 *   structurally distinct constraints sharing the dharmasastra_corpus kernel
 *   — sibling readings (reformist_contextual, abolitionist_rejection) are
 *   separate files with their own epsilon and stakeholder sets, not
 *   alternative measurements of this one. Under this reading's own lights,
 *   the standing arrangement is the literal enforcement of varna/jati
 *   distinctions in ritual access, education, occupation, and marriage;
 *   extraction is authored as high because this reading treats hierarchy
 *   assignment as non-negotiable birth-status, which is precisely what
 *   generates the expansive victim set (Dalits, Shudras, women) and the
 *   concentrated beneficiary set (Brahmin lineages, upper-caste landholders,
 *   temple authorities) named in the kernel context.
 *
 * KEY AGENTS:
 *   - brahmin_priestly_lineages: agenda_setter/beneficiary (institutional/arbitrage) — administers and collects from the literal hierarchy
 *   - upper_caste_landholders: beneficiary (powerful/constrained) — benefits materially from naturalized labor subordination
 *   - temple_and_matha_authorities: agenda_setter/beneficiary (institutional/arbitrage) — enforces ritual/educational exclusion
 *   - dalit_communities: payer (powerless/trapped) — bears exclusion from temple, education, occupation
 *   - shudra_laborers: payer (powerless/trapped) — bears assigned servile duty
 *   - women_across_varnas: payer (powerless/identity_locked) — bears gendered restriction regardless of varna
 *   - reformist_scholars: excluded (moderate/constrained) — contest literal reading, denied interpretive authority
 *   - colonial_and_postcolonial_courts: observer (institutional/analytical) — adjudicates without belonging to either camp
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_corpus__orthodox_literalist, 0.81).
domain_priors:suppression_score(dharmasastra_corpus__orthodox_literalist, 0.87).
domain_priors:theater_ratio(dharmasastra_corpus__orthodox_literalist, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, extractiveness, 0.81).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, suppression_requirement, 0.87).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__orthodox_literalist, tangled_rope).
narrative_ontology:human_readable(dharmasastra_corpus__orthodox_literalist, "Orthodox Literalist Reading of Dharmasastra Varna/Jati Hierarchy as Eternal Revealed Law").
narrative_ontology:topic_domain(dharmasastra_corpus__orthodox_literalist, "religious_law/textual_interpretation/normative_authority").

domain_priors:requires_active_enforcement(dharmasastra_corpus__orthodox_literalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__orthodox_literalist, '30ff58dd-e970-4a5a-b8f2-c092ef8f2145').
narrative_ontology:cs_kernel_codification('30ff58dd-e970-4a5a-b8f2-c092ef8f2145', fixed_text).
narrative_ontology:cs_authority_grounding('30ff58dd-e970-4a5a-b8f2-c092ef8f2145', lineage).
narrative_ontology:cs_interpretation_layer_present('30ff58dd-e970-4a5a-b8f2-c092ef8f2145').
narrative_ontology:cs_reading_relation('30ff58dd-e970-4a5a-b8f2-c092ef8f2145', dharmasastra_corpus__reformist_contextual, coexists_with).
narrative_ontology:cs_reading_relation('30ff58dd-e970-4a5a-b8f2-c092ef8f2145', dharmasastra_corpus__abolitionist_rejection, forecloses).
narrative_ontology:cs_axiom('30ff58dd-e970-4a5a-b8f2-c092ef8f2145', foundational, varna_prescriptions_are_eternal_and_nonseparable_from_dharma).
narrative_ontology:cs_axiom_status(varna_prescriptions_are_eternal_and_nonseparable_from_dharma, holdable).
narrative_ontology:cs_axiom_grounding('30ff58dd-e970-4a5a-b8f2-c092ef8f2145', varna_prescriptions_are_eternal_and_nonseparable_from_dharma, theological).
narrative_ontology:cs_axiom('30ff58dd-e970-4a5a-b8f2-c092ef8f2145', foundational, literal_textual_observance_is_the_only_legitimate_interpretive_mode).
narrative_ontology:cs_axiom_status(literal_textual_observance_is_the_only_legitimate_interpretive_mode, holdable).
narrative_ontology:cs_axiom_grounding('30ff58dd-e970-4a5a-b8f2-c092ef8f2145', literal_textual_observance_is_the_only_legitimate_interpretive_mode, conventional).
narrative_ontology:cs_reference_frame('30ff58dd-e970-4a5a-b8f2-c092ef8f2145', sanatana_varna_order_as_revealed).
narrative_ontology:cs_drift_state('30ff58dd-e970-4a5a-b8f2-c092ef8f2145', post_constitutional_equality_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('30ff58dd-e970-4a5a-b8f2-c092ef8f2145', '').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__orthodox_literalist, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, brahmin_priestly_lineages).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, upper_caste_landholders).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, temple_and_matha_authorities).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, dalit_communities).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, shudra_laborers).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, women_across_varnas).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__orthodox_literalist, varna_order_as_cosmic_dharma).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__orthodox_literalist, shruti_smriti_continuity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Transmit and adjudicate the dharmasastra corpus (Manusmriti and related texts) as revealed, eternal law. Control ritual monopoly, education access, and the interpretive apparatus that declares the varna hierarchy literal and binding. Their social position, income, and ritual authority all derive from the hierarchy's literal enforcement; they administer the standard and collect from it simultaneously.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, brahmin_priestly_lineages, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(dharmasastra_corpus__orthodox_literalist, brahmin_priestly_lineages, beneficiary).

% Hold land, labor, and marriage-market advantages that the literal hierarchy naturalizes as cosmically ordained rather than historically accumulated. Benefit from a supply of bonded or underpaid Shudra and Dalit labor whose subordination the texts frame as dharmic duty, not exploitation.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, upper_caste_landholders, beneficiary,
    powerful, generational, constrained, regional).

% Administer temple entry, ritual eligibility, and educational gatekeeping according to literal varna/jati rules. Enforce exclusion at the level of physical access (temple thresholds, water sources, seating) and collect the social and material capital that flows from being the recognized custodians of eternal law.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, temple_and_matha_authorities, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(dharmasastra_corpus__orthodox_literalist, temple_and_matha_authorities, beneficiary).

% Historically and presently excluded from temple entry, Vedic education, and many occupations under the literal reading, which frames this exclusion as inherent to their birth-status rather than as imposed subordination. Exit requires either conversion, migration, or legal recourse under a competing (secular or reformist) framework that the literalist reading treats as illegitimate departure from revealed order.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, dalit_communities, payer,
    powerless, generational, trapped, national).

% Assigned service and labor duties as their dharmic function under the literal reading, which forecloses claims to ritual, educational, or occupational mobility as violations of their nature rather than as denied opportunity. Economic dependency on upper-caste landholders compounds the textual exclusion with material coercion.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, shudra_laborers, payer,
    powerless, generational, trapped, regional).

% Subject to prescriptions (e.g. pativrata duty, restricted access to independent ritual agency, inheritance limits) that the literalist reading treats as eternal and non-negotiable regardless of varna. Exit is complicated by internalized religious identity as much as by external enforcement — leaving the framework can mean leaving one's community and cosmology, not merely a legal status.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, women_across_varnas, payer,
    powerless, biographical, identity_locked, national).

% Argue the caste prescriptions are time-bound social regulation (desa-kala-dependent) separable from dharma's ethical core, and are excluded from literalist interpretive authority as heterodox or inauthentic readers of the tradition. Their textual arguments are heard in academic and reform circles but not admitted as legitimate within the orthodox interpretive chain.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, reformist_scholars, excluded,
    moderate, generational, constrained, national).

% Adjudicated (and continue to adjudicate) the legal status of caste-based exclusion, sometimes codifying literalist readings into personal law, sometimes overriding them via constitutional equality provisions. Their rulings shift the enforcement landscape without themselves belonging to either interpretive camp.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, colonial_and_postcolonial_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, textually anchored social order that assigns ritual role, occupation, and marriage eligibility without renegotiation in each generation, reducing status contestation within the reading's own framework.
% TRANSFER_FUNCTION: Moves ritual purity, educational access, land, labor claims, and marriage-market position from Dalits, Shudras, and women toward Brahmin lineages and upper-caste landholders, justified as the fulfillment rather than the transfer of dharmic duty.
% ABSENT_VOICES: Dalit and Shudra communities and women across varnas experience the exclusions the texts prescribe but are not admitted as authoritative interpreters of what the texts mean or whether they still bind; reformist scholars who would contest literal observance are excluded from the orthodox interpretive chain entirely.
% DISAPPEARANCE_RATIONALE: If the literalist reading's authority collapsed overnight, temple entry, educational access, and occupational assignment would cease to be adjudicated by textual varna/jati status; land and ritual capital currently legitimated by 'eternal law' would face open contestation as historically accumulated advantage rather than cosmically ordained position — this is precisely the rearrangement Dalit rights movements and reformist campaigns have sought.
% FOUNDING_PROBLEM: Provide a comprehensive normative order for ritual, social, and legal conduct in a context where written codification of duty, purity, and social role was seen as necessary to stabilize a complex, stratified agrarian society.
% FOUNDING_PROBLEM_CORROBORATION: Orthodox authorities (brahmin lineages, matha institutions) attest the founding problem is permanently live because dharma is eternal by definition. Outside the benefiting parties: constitutional courts, Dalit rights organizations, and reformist scholars attest the social-stabilization problem the texts addressed was historically specific and has been substantially superseded by modern legal and economic institutions — the literal hierarchy's persistence is read by these outside observers as inertial/extractive rather than functionally necessary.
narrative_ontology:disappearance_verdict(dharmasastra_corpus__orthodox_literalist, world_rearranges).
narrative_ontology:founding_problem_status(dharmasastra_corpus__orthodox_literalist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dharmasastra_corpus__orthodox_literalist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dharmasastra_corpus__orthodox_literalist, 'none', 1).
narrative_ontology:epsilon_provenance(dharmasastra_corpus__orthodox_literalist, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dharmasastra_corpus__orthodox_literalist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dharmasastra_corpus__orthodox_literalist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dharmasastra_corpus__orthodox_literalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.81) is authored high because, under this reading's own terms, the entire material and social apparatus of caste advantage is presented as an unchangeable consequence of birth rather than a negotiable social arrangement — the literalism is precisely what forecloses redistribution or renegotiation. Suppression (0.87) is authored even higher than extraction because maintaining literal observance against centuries of social change, legal challenge, and internal reform pressure requires continuous active enforcement: temple-entry disputes, caste-based social sanction, endogamy enforcement, and periodic communal violence. Theater ratio rises over the measured interval (0.20 to 0.42) reflecting that as legal and constitutional equality frameworks have encroached on literal enforcement's practical reach, a growing share of the literalist apparatus's activity has shifted from substantive control (direct denial of land, education, temple access) toward symbolic/ritual assertion of hierarchy where substantive enforcement has become legally unavailable. Accessibility collapse (0.62) is moderate rather than mountain-level because, unlike a genuine natural law, alternative dharmic and legal readings have always existed and have gained ground — the literalist reading's claim to being the only legitimate reading has never achieved total closure.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter/beneficiary seats (Brahmin lineages, temple authorities), the arrangement computes as genuine coordination: a stable, textually-grounded social order preventing status renegotiation chaos. From the payer seats (Dalit communities, Shudra laborers, women), the identical structure computes as enforced extraction requiring continuous active suppression to hold against contestation. The engine's per-seat computation is expected to diverge sharply here — that divergence is the data point, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Brahmin lineages and temple/matha authorities sit at the beneficiary end of directionality: they administer the interpretive apparatus AND collect its social/material yield (ritual monopoly, educational gatekeeping, social deference) — arbitrage-grade exit because their position is portable across changing legal regimes (their authority survives even where enforcement weakens). Upper-caste landholders are beneficiaries at one remove: they do not administer doctrine but capture the labor-market and marriage-market advantage the doctrine naturalizes. Dalit communities and Shudra laborers sit at the full-target end: trapped exit options because birth-status assignment under the literal reading is precisely what forecloses exit through achievement or relocation within the framework's own terms; escape requires exiting the framework itself (conversion, legal appeal to a competing authority, migration). Women across varnas are directionality-targets with identity_locked exit specifically because the suppression mechanism here is partly internalized — leaving prescribed gender roles can mean leaving one's cosmological identity and community standing, not merely a legal status, which is qualitatively different from the more externally-trapped situation of caste-labor assignment.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem/disappearance_verdict mismatch is central to this reading's classification: the orthodox literalist position holds founding_problem_status as permanently 'live' (dharma is eternal by definition, so the problem it solves cannot become dead) while disappearance_verdict is 'world_rearranges' — outside corroborators (courts, rights organizations, reformist scholars) read this combination as exactly the zombie-mandate signature: an arrangement whose stated justification cannot ever expire by its own logic, while its removal would demonstrably rearrange concrete institutional practice (temple access, land tenure, marriage markets). This is the mandatrophy pattern the tangled_rope classification is built to catch: real coordination benefit for those inside the beneficiary seats, layered extraction for those in the payer seats, sustained only by continuous active enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    eternality_claim_vs_historical_construction,
    'Are the varna/jati prescriptions genuinely eternal/revealed within the tradition''s own epistemics, or is the eternality claim itself a historically constructed doctrine that serves identifiable beneficiaries (Brahmin interpretive authority, upper-caste material advantage)?',
    'Comparative textual-historical analysis of dharmasastra redaction layers, cross-referencing archaeological and epigraphic evidence of caste practice variation across regions and eras against the claim of a single unchanging revealed order; also examine whether other smriti traditions within the same broader corpus treat time-bound (desa-kala) qualification as legitimate, which would undercut universal eternality.',
    'If the eternality claim is shown to be a doctrinal innovation rather than a continuous revealed truth, the literalist reading''s foundational axiom loses its grounding-type distinction from conventional/institutional claims, which would materially strengthen the reformist reading''s structural position and weaken this reading''s claim to be anything other than an extractive interpretive monopoly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eternality_claim_vs_historical_construction, conceptual, 'Whether the eternal/revealed status of caste prescriptions is a genuine doctrinal feature or a historically constructed legitimating claim.').

omega_variable(
    internalized_vs_structural_suppression_gendered,
    'For women across varnas, is the measured suppression primarily structural (legal/economic exclusion enforced externally) or internalized (identity fusion with prescribed roles making exit unthinkable even absent external barriers)?',
    'Post-exit trajectory analysis: track whether women who gain legal and economic independence from prescribed roles (via changed law, migration, or economic opportunity) continue to experience constraint-consistent behavior and self-restriction absent external enforcement — persistence would indicate substantial internalization.',
    'If suppression proves substantially internalized, the effective suppression this reading exerts on women is higher than the structural exclusion measure alone suggests, since the target carries the constraint even where external enforcement has been removed — this would sharpen rather than soften the extraction reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression_gendered, empirical, 'Structural vs. internalized suppression mechanism specifically for the women_across_varnas stakeholder group.').

omega_variable(
    coupling_between_ritual_status_and_material_extraction,
    'Is the ritual/purity component of the hierarchy (temple access, commensality rules) structurally separable from the material extraction component (land, labor, marriage-market advantage), or does the literalist reading require both to hold together as a single indivisible system?',
    'Examine historical and contemporary cases where ritual status has been contested or reformed (e.g., temple entry movements) while material caste advantage persisted, or vice versa, to test whether the two components move independently.',
    'If separable, some of the measured extraction may be attributable to material caste dynamics independent of the textual literalism itself, meaning the literalist reading''s distinctive contribution is narrower than the aggregate metric suggests; if inseparable, the literalist reading is doing the full work of legitimating both.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coupling_between_ritual_status_and_material_extraction, conceptual, 'Whether ritual-status and material-extraction components of the hierarchy are structurally coupled or independently variable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__orthodox_literalist, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dhar_tr_t0, dharmasastra_corpus__orthodox_literalist, theater_ratio, 0, 0.2).
narrative_ontology:measurement(dhar_tr_t40, dharmasastra_corpus__orthodox_literalist, theater_ratio, 40, 0.25).
narrative_ontology:measurement(dhar_tr_t80, dharmasastra_corpus__orthodox_literalist, theater_ratio, 80, 0.3).
narrative_ontology:measurement(dhar_tr_t120, dharmasastra_corpus__orthodox_literalist, theater_ratio, 120, 0.36).
narrative_ontology:measurement(dhar_tr_t160, dharmasastra_corpus__orthodox_literalist, theater_ratio, 160, 0.4).
narrative_ontology:measurement(dhar_tr_t200, dharmasastra_corpus__orthodox_literalist, theater_ratio, 200, 0.42).

% Extraction over time
narrative_ontology:measurement(dhar_be_t0, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(dhar_be_t40, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 40, 0.74).
narrative_ontology:measurement(dhar_be_t80, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 80, 0.78).
narrative_ontology:measurement(dhar_be_t120, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 120, 0.8).
narrative_ontology:measurement(dhar_be_t160, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 160, 0.81).
narrative_ontology:measurement(dhar_be_t200, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 200, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(dhar_su_t0, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(dhar_su_t40, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 40, 0.78).
narrative_ontology:measurement(dhar_su_t80, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 80, 0.82).
narrative_ontology:measurement(dhar_su_t120, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 120, 0.85).
narrative_ontology:measurement(dhar_su_t160, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 160, 0.86).
narrative_ontology:measurement(dhar_su_t200, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 200, 0.87).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
