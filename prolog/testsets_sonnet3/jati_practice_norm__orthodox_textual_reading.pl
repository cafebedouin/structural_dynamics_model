% ============================================================================
% CONSTRAINT STORY: jati_practice_norm__orthodox_textual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jati_practice_norm__orthodox_textual_reading, []).

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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: jati_practice_norm__orthodox_textual_reading
 *   human_readable: Orthodox Textual Reading: Jati Boundaries as Fixed Scriptural Varna Order
 *   domain: social/religious/political_economy
 *
 * SUMMARY:
 *   This story is the orthodox textual reading of the jati_practice_norm
 *   kernel: the claim that jati boundaries are a direct, fixed derivation
 *   from a scriptural varna framework (the four-fold
 *   Brahmin/Kshatriya/Vaishya/Shudra order plus an excluded 'outcaste'
 *   category), such that deviation from an assigned occupational-ritual
 *   station constitutes pollution requiring social exclusion or ritual
 *   correction. This reading treats the mapping as textually settled and
 *   therefore not subject to local renegotiation or administrative
 *   construction — the two claims made by the sibling readings in this kernel
 *   (localized_practice_reading, colonial_census_reading). Under this
 *   reading's own terms, the framework is a coordination device for
 *   cosmic-social order; under the authored metrics, the framework operates
 *   as a near-hereditary extraction and exclusion mechanism whose
 *   beneficiaries are the interpretive authorities and dominant jatis, and
 *   whose victims are the jatis assigned stigmatized, polluting, or excluded
 *   occupations across generations with structurally blocked mobility.
 *
 * KEY AGENTS:
 *   - brahmin_priestly_lineages: interpretive authority and ritual gatekeepers (institutional/arbitrage) — set and enforce the boundary, collect deference and resource
 *   - dominant_landholding_jatis: extract labor and deference under the fixed-order justification (powerful/constrained)
 *   - temple_authorities: enforce physical exclusion from sacred space (institutional/arbitrage)
 *   - sanitation_labor_jatis, leatherwork_jatis, manual_scavenging_jatis: hereditarily assigned polluting occupations, trapped exit, bear the extraction (powerless/trapped)
 *   - intercaste_offspring: absorb boundary-crossing penalties as inherited status (powerless/trapped)
 *   - reformist_religious_movements: excluded counter-interpretive voice (organized/constrained)
 *   - comparative_textual_scholars: analytical observer documenting the constructed, layered nature of the mapping (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jati_practice_norm__orthodox_textual_reading, 0.87).
domain_priors:suppression_score(jati_practice_norm__orthodox_textual_reading, 0.9).
domain_priors:theater_ratio(jati_practice_norm__orthodox_textual_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, extractiveness, 0.87).
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jati_practice_norm__orthodox_textual_reading, snare).
narrative_ontology:human_readable(jati_practice_norm__orthodox_textual_reading, "Orthodox Textual Reading: Jati Boundaries as Fixed Scriptural Varna Order").
narrative_ontology:topic_domain(jati_practice_norm__orthodox_textual_reading, "social/religious/political_economy").

domain_priors:requires_active_enforcement(jati_practice_norm__orthodox_textual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jati_practice_norm__orthodox_textual_reading, '36c35766-74e3-4f60-bd90-3a72c016a9f0').
narrative_ontology:cs_kernel_codification('36c35766-74e3-4f60-bd90-3a72c016a9f0', fixed_text).
narrative_ontology:cs_authority_grounding('36c35766-74e3-4f60-bd90-3a72c016a9f0', lineage).
narrative_ontology:cs_interpretation_layer_present('36c35766-74e3-4f60-bd90-3a72c016a9f0').
narrative_ontology:cs_reading_relation('36c35766-74e3-4f60-bd90-3a72c016a9f0', jati_practice_norm__colonial_census_reading, coexists_with).
narrative_ontology:cs_reading_relation('36c35766-74e3-4f60-bd90-3a72c016a9f0', jati_practice_norm__localized_practice_reading, forecloses).
narrative_ontology:cs_axiom('36c35766-74e3-4f60-bd90-3a72c016a9f0', foundational, varna_jati_mapping_is_scripturally_fixed).
narrative_ontology:cs_axiom_status(varna_jati_mapping_is_scripturally_fixed, holdable).
narrative_ontology:cs_axiom_grounding('36c35766-74e3-4f60-bd90-3a72c016a9f0', varna_jati_mapping_is_scripturally_fixed, theological).
narrative_ontology:cs_axiom('36c35766-74e3-4f60-bd90-3a72c016a9f0', foundational, occupational_station_is_hereditary_and_immutable).
narrative_ontology:cs_axiom_status(occupational_station_is_hereditary_and_immutable, holdable).
narrative_ontology:cs_axiom_grounding('36c35766-74e3-4f60-bd90-3a72c016a9f0', occupational_station_is_hereditary_and_immutable, theological).
narrative_ontology:cs_axiom('36c35766-74e3-4f60-bd90-3a72c016a9f0', secondary, ritual_pollution_from_boundary_crossing_is_cosmologically_real).
narrative_ontology:cs_axiom_status(ritual_pollution_from_boundary_crossing_is_cosmologically_real, holdable).
narrative_ontology:cs_axiom_grounding('36c35766-74e3-4f60-bd90-3a72c016a9f0', ritual_pollution_from_boundary_crossing_is_cosmologically_real, theological).
narrative_ontology:cs_reference_frame('36c35766-74e3-4f60-bd90-3a72c016a9f0', classical_dharmashastra_varna_order).
narrative_ontology:cs_drift_state('36c35766-74e3-4f60-bd90-3a72c016a9f0', post_constitutional_abolition_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('36c35766-74e3-4f60-bd90-3a72c016a9f0', '').
narrative_ontology:cs_kernel_id(jati_practice_norm__orthodox_textual_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__orthodox_textual_reading, brahmin_priestly_lineages).
narrative_ontology:constraint_beneficiary(jati_practice_norm__orthodox_textual_reading, dominant_landholding_jatis).
narrative_ontology:constraint_beneficiary(jati_practice_norm__orthodox_textual_reading, temple_authorities).
narrative_ontology:constraint_victim(jati_practice_norm__orthodox_textual_reading, sanitation_labor_jatis).
narrative_ontology:constraint_victim(jati_practice_norm__orthodox_textual_reading, leatherwork_jatis).
narrative_ontology:constraint_victim(jati_practice_norm__orthodox_textual_reading, manual_scavenging_jatis).
narrative_ontology:constraint_victim(jati_practice_norm__orthodox_textual_reading, intercaste_offspring).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and transmit the scriptural varna texts, adjudicate ritual purity disputes, and perform the rites that certify status. Their monopoly on Sanskritic textual authority is the mechanism by which the boundary is declared fixed rather than negotiable; they collect fees, land grants, and deference for this function and bear none of the labor stigma the framework assigns downstream.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, brahmin_priestly_lineages, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(jati_practice_norm__orthodox_textual_reading, brahmin_priestly_lineages, beneficiary).

% Occupy a mid-to-upper position in the varna-derived hierarchy that the orthodox reading treats as scripturally settled. They extract labor, land-tenancy obligations, and social deference from jatis assigned polluting or servile occupations, and they invoke the fixed textual order to deny that this arrangement is a local, negotiable, or historically contingent one.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, dominant_landholding_jatis, beneficiary,
    powerful, generational, constrained, regional).

% Control physical access to sacred space and ritual participation, enforcing entry bans and separate-queue arrangements on the theory that certain jatis carry inherited ritual pollution. This gatekeeping function generates revenue, land endowments, and social authority, and depends entirely on the varna framework being treated as scripturally fixed rather than as an administered local custom.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, temple_authorities, agenda_setter,
    institutional, civilizational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(jati_practice_norm__orthodox_textual_reading, temple_authorities, beneficiary).

% Assigned hereditary sanitation and waste-handling occupations that the orthodox framework designates as inherently polluting, which is then read backward to justify their exclusion from wells, temples, schools, and intermarriage. Occupational inheritance is treated as scripturally mandated rather than as an assignment; attempts to change occupation are met with social and sometimes physical enforcement.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, sanitation_labor_jatis, payer,
    powerless, generational, trapped, local).

% Hereditarily assigned to hide-processing and leatherwork, classified as ritually impure under the orthodox varna reading because the work involves animal carcasses. This classification blocks entry into landholding, priestly, or trading occupations regardless of individual skill or preference, and intermarriage across the boundary is treated as a ritual violation requiring purification of the 'higher' party.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, leatherwork_jatis, payer,
    powerless, generational, trapped, local).

% Perform the most stigmatized cleaning labor, treated under the orthodox reading as occupying a position at or below the four-varna scheme entirely ('outcaste'). They face the most severe accessibility collapse: residential segregation, denial of common water sources, and violent enforcement of distance norms, justified as maintaining scriptural ritual order rather than as social control of a labor pool.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, manual_scavenging_jatis, payer,
    powerless, immediate, trapped, local).

% Born of unions that cross jati lines the orthodox scriptural reading treats as fixed and impermeable. Classical textual sources (e.g., mixed-varna progeny doctrines) assign them to newly stigmatized sub-categories rather than allowing fluid reclassification, converting a single boundary-crossing event into a permanently inherited status for descendants.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, intercaste_offspring, payer,
    powerless, biographical, trapped, local).

% Bhakti, Buddhist, Jain, and later reform movements have historically argued the varna-jati mapping is a corrupted or non-scriptural accretion rather than a fixed textual mandate. Under the orthodox reading's own terms, these movements are heterodox and their textual counter-readings are excluded from the authoritative interpretive lineage rather than engaged as a live alternative.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, reformist_religious_movements, excluded,
    organized, generational, constrained, national).

% Study the textual corpus (Manusmriti, Dharmashastra commentaries, regional smriti traditions) and document that the varna-to-jati mapping described as fixed is itself a much later, contested, and regionally variable interpretive construction layered onto older and vaguer textual material. Their findings are available to, but not authoritative for, the priestly interpretive lineage.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, comparative_textual_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jati_practice_norm__orthodox_textual_reading, diffuse).
narrative_ontology:fixing_cost_class(jati_practice_norm__orthodox_textual_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In its own terms, the orthodox reading claims to solve a cosmological-social coordination problem: assigning every person a fixed ritual-occupational station within a divinely ordained order, so that social and ritual life proceeds without contestation over role or status.
% TRANSFER_FUNCTION: Moves labor, land-tenancy obligations, ritual deference, and social honor from jatis assigned polluting or servile occupations to priestly and landholding jatis, while moving physical and social costs (segregation, exclusion from resources, blocked mobility) onto the assigned-polluting jatis and their descendants in perpetuity.
% ABSENT_VOICES: Reformist religious movements and the assigned-polluting jatis themselves have historically produced counter-readings of the same textual corpus (e.g., arguing varna was originally about aptitude, not birth, or that later smriti layers are corrupted); the orthodox interpretive lineage treats these as heterodox rather than admitting them into the authoritative reading, structurally excluding the parties most affected from the interpretive process that defines their status.
% DISAPPEARANCE_RATIONALE: If the orthodox textual reading's authority collapsed overnight, occupational assignment, marriage eligibility, temple access, and settlement patterns organized around it would lose their justificatory basis; landholding jatis would lose a cost-free supply of stigmatized labor and priestly lineages would lose their gatekeeping revenue and status — this is a live, presently-operating arrangement, not a residual belief.
% FOUNDING_PROBLEM: Presented within the tradition as solving the problem of maintaining ritual purity and cosmic/social order (dharma) by assigning every person a fixed station corresponding to their birth, preventing what the framework frames as chaotic mixing of duties and pollution of sacred space.
% FOUNDING_PROBLEM_CORROBORATION: Priestly lineages and dominant landholding jatis attest the framework encodes a still-live scriptural mandate. Comparative textual scholars (outside the beneficiary set) attest that the rigid varna-to-jati mapping is a later interpretive layering rather than a stable ancient mandate, and that the earliest textual material is far vaguer and more contested than the orthodox reading claims; reformist religious movements, also outside the beneficiary set, have independently made the same genealogical argument for over a millennium.
narrative_ontology:disappearance_verdict(jati_practice_norm__orthodox_textual_reading, world_rearranges).
narrative_ontology:founding_problem_status(jati_practice_norm__orthodox_textual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jati_practice_norm__orthodox_textual_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jati_practice_norm__orthodox_textual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jati_practice_norm__orthodox_textual_reading, 0.87, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jati_practice_norm__orthodox_textual_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jati_practice_norm__orthodox_textual_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jati_practice_norm__orthodox_textual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.87) because, under this reading, occupational assignment and its attendant social/economic consequences are treated as immutable and inherited rather than negotiable, which forecloses exit for the assigned jatis across generations — this is precisely what the localized_practice_reading disputes as empirically false (jati boundaries in practice show continuous renegotiation and jati proliferation) and what makes the orthodox reading structurally distinct. Suppression is authored even higher (0.90) because enforcement includes not just economic exclusion but physical exclusion from wells, temples, and settlements, backed historically and presently by social and sometimes violent sanction. Accessibility collapse (0.78) reflects that once a jati's textual classification is accepted as scripturally fixed, occupational and marital alternatives are treated as unthinkable rather than merely costly. Resistance (0.72) is authored high because reform movements, legal abolition of untouchability, and affected-jati mobilization have contested this reading continuously for over a century — this is not a settled mountain, it is a contested, actively defended snare.
 *
 * PERSPECTIVAL GAP:
 *   From the priestly/temple/dominant-jati seats, the orthodox reading is a coordination mechanism realizing a scripturally mandated cosmic order — properly a tangled_rope or even a rope claim from inside the tradition. From the assigned-polluting jati seats, the identical structure computes as an enforced, inherited extraction with no meaningful exit: a snare. The engine should compute this divergence from the structural declarations (trapped exit, hereditary assignment, blocked mobility) rather than from either seat's self-description.
 *
 * DIRECTIONALITY LOGIC:
 *   Priestly lineages and temple authorities sit at the extreme beneficiary end: they administer the interpretive apparatus and collect resource and deference from it while bearing none of its costs (d near 0). Dominant landholding jatis are beneficiaries at one remove: they extract labor and deference justified by the framework without controlling its interpretation. The assigned-polluting jatis and intercaste offspring sit at the extreme target end: trapped exit, hereditary assignment, and the accessibility collapse of alternative status routes push d near 1 regardless of individual preference or ability. Reformist movements are excluded rather than coordinated — their exclusion from the authoritative interpretive lineage is part of what keeps the orthodox reading's boundary from being renegotiated the way the localized_practice_reading describes.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (maintaining a cosmological-ritual order) is treated by the orthodox reading as eternally live, which is exactly the move that keeps a historically contingent, later-layered interpretive scheme immune from revision. Comparative textual scholarship and the reformist tradition's own centuries-old counter-arguments — both outside the beneficiary set — corroborate that the rigid, birth-fixed varna-to-jati mapping this reading asserts is itself a later interpretive layering, not a stable ancient mandate. That mismatch between an asserted-live founding problem and an outside-corroborated, contested genealogical status is the mandatrophy signal here: the arrangement persists at full extractive intensity while its own claimed textual grounding is independently disputed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_fixity_vs_interpretive_layering,
    'Does the scriptural corpus itself mandate a fixed, birth-determined varna-to-jati mapping, or is that mapping a later interpretive layering onto vaguer, contested, and regionally variable earlier material?',
    'Comparative philological analysis across strata of the Dharmashastra corpus and regional commentarial traditions, cross-checked against the independent claims of reformist religious traditions that have disputed the birth-fixity reading for over a millennium.',
    'If the mapping is substantially a later interpretive construction rather than an original mandate, the orthodox reading''s own founding-problem claim (live scriptural mandate) is undermined from within its own textual tradition, strengthening the mandatrophy reading and supporting reclassification toward pure extraction rather than contested coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_fixity_vs_interpretive_layering, empirical, 'Whether the fixed varna-jati mapping is original scripture or later interpretive accretion.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly do the three readings of the jati_practice_norm kernel disagree — on the FACTS of how jati boundaries have historically operated, or on which INTERPRETIVE AUTHORITY gets to fix their meaning?',
    'This is routed here per the committer-frame rule rather than resolved within this story. The disagreement is located at the interpretive-authority layer: the orthodox reading treats the priestly textual lineage as the sole legitimate interpreter and treats the resulting categories as fixed; the localized_practice_reading treats local practice and continuous renegotiation as the operative reality regardless of textual claims; the colonial_census_reading treats an external administrative apparatus as having stabilized categories that were previously fluid. A sibling reading adopting a different interpretive authority would change beneficiaries, victims, ε, and type for that reading without changing this one.',
    'Clarifies that this story does not adjudicate the kernel contest; it commits to one interpretive-authority framing (textual/priestly) and inherits high extraction and blocked mobility as a structural consequence of that framing''s claimed fixity, not as a finding about which reading is empirically correct overall.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Locates the inter-reading disagreement at the interpretive-authority layer, not the factual layer.').

omega_variable(
    false_summit_natural_order_claim,
    'Is the ritual-pollution/purity framework a genuine natural-cosmological fact (as the orthodox tradition asserts) or a constructed hierarchy that benefits identifiable priestly and landholding groups?',
    'Cross-tradition comparison (non-Vedic Indic traditions, e.g. some Buddhist and Jain lineages, that reject inherited ritual pollution as cosmologically necessary) and material analysis of who captures labor and deference under the framework versus who bears its costs.',
    'If the pollution framework is constructed rather than natural, treating it as a mountain-like fixed cosmic order is a false summit; the identifiable, concentrated beneficiary set (priestly lineages, temple authorities, dominant jatis) combined with a clearly excluded, powerless victim set supports the snare classification authored here over any naturalized reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_order_claim, conceptual, 'Whether the ritual-purity order is natural cosmology or a constructed, beneficiary-serving hierarchy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_practice_norm__orthodox_textual_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_tr_t0, jati_practice_norm__orthodox_textual_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(jati_tr_t20, jati_practice_norm__orthodox_textual_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(jati_tr_t40, jati_practice_norm__orthodox_textual_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement(jati_tr_t60, jati_practice_norm__orthodox_textual_reading, theater_ratio, 60, 0.33).
narrative_ontology:measurement(jati_tr_t80, jati_practice_norm__orthodox_textual_reading, theater_ratio, 80, 0.37).
narrative_ontology:measurement(jati_tr_t100, jati_practice_norm__orthodox_textual_reading, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(jati_be_t0, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(jati_be_t20, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 20, 0.78).
narrative_ontology:measurement(jati_be_t40, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 40, 0.83).
narrative_ontology:measurement(jati_be_t60, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 60, 0.85).
narrative_ontology:measurement(jati_be_t80, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 80, 0.86).
narrative_ontology:measurement(jati_be_t100, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 100, 0.87).

% Suppression requirement over time
narrative_ontology:measurement(jati_su_t0, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(jati_su_t20, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 20, 0.83).
narrative_ontology:measurement(jati_su_t40, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 40, 0.86).
narrative_ontology:measurement(jati_su_t60, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 60, 0.88).
narrative_ontology:measurement(jati_su_t80, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 80, 0.89).
narrative_ontology:measurement(jati_su_t100, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 100, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jati_practice_norm__orthodox_textual_reading, identity_coordination).
narrative_ontology:affects_constraint(jati_practice_norm__orthodox_textual_reading, jati_practice_norm__localized_practice_reading).
narrative_ontology:affects_constraint(jati_practice_norm__orthodox_textual_reading, jati_practice_norm__colonial_census_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the jati_practice_norm kernel (a natural-language label covering structurally distinct claims about the origin and operation of jati boundaries). orthodox_textual_reading claims scriptural fixity and high hereditary extraction with blocked mobility (snare, authored here). localized_practice_reading (sibling file) claims jati boundaries are local coordination norms under continuous renegotiation and proliferation — a substantially lower-extraction, more rope-like structure. colonial_census_reading (sibling file) claims the rigid, enumerable categories were substantially produced and stabilized by external colonial administrative classification for governance legibility — a tangled_rope-like structure whose beneficiary is the administrative apparatus rather than the priestly lineage. Each carries its own ε, beneficiary/victim set, and claimed type; they are linked here rather than merged per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
