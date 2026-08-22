% ============================================================================
% CONSTRAINT STORY: imposition_pathway_kernel__hybrid_cascade_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_pathway_kernel__hybrid_cascade_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: imposition_pathway_kernel__hybrid_cascade_reading
 *   human_readable: State-Manufactured Fringe Cascade (Meiji-Era Commitment Displacement)
 *   domain: historical_sociology/state_formation/commitment_systems
 *
 * SUMMARY:
 *   The Meiji government's 1870s decrees mandating Western dress, haircuts,
 *   and conscription for state employees and military personnel created an
 *   artificial fringe — a cohort forced to adopt new commitments. This
 *   artificial fringe then became the vector for organic climb: their
 *   children and social networks adopted voluntarily, the new commitments
 *   spread through status emulation, and within two generations the imposed
 *   commitments had become the society's dominant commitments. The constraint
 *   is the hybrid cascade mechanism itself: top-down imposition manufactures
 *   the fringe that enables climb. The state_bureaucracy and
 *   military_officer_corps are agenda_setters (they administer the
 *   imposition) and beneficiaries (they capture status and material rents
 *   from the new order). The traditional_samurai_class and
 *   domain_loyal_retainers are primary payers — their status, stipends, and
 *   identity are extracted. The rural_population_subject_to_conscription are
 *   payers with trapped exit. The imperial_institution is a
 *   vindicated_proposition (the restoration's legitimacy doctrine) — it
 *   collects symbolic capital but its material rent capture is contested
 *   (omega). This reading instantiates the hybrid_cascade_reading of the
 *   imposition_pathway_kernel: override initiates (state decree creates
 *   artificial fringe), climb completes (organic diffusion from that fringe).
 *
 * KEY AGENTS:
 *   - state_bureaucracy: Primary agenda_setter/beneficiary (institutional/trapped) — administers imposition, captures new status rents
 *   - military_officer_corps: Primary agenda_setter/beneficiary (institutional/identity_locked) — enforces conscription, becomes new elite
 *   - imperial_institution: Vindicated proposition / contested beneficiary (institutional/identity_locked) — legitimacy doctrine, material capture ambiguous
 *   - traditional_samurai_class: Primary payer (powerful/identity_locked) — loses stipends, status, identity
 *   - domain_loyal_retainers: Primary payer (organized/identity_locked) — loses feudal bonds, domain identity
 *   - rural_population_subject_to_conscription: Payer (powerless/trapped) — bodies and labor extracted
 *   - historical_sociologist: Observer (analytical/analytical) — sees full cascade structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_pathway_kernel__hybrid_cascade_reading, 0.78).
domain_priors:suppression_score(imposition_pathway_kernel__hybrid_cascade_reading, 0.72).
domain_priors:theater_ratio(imposition_pathway_kernel__hybrid_cascade_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_pathway_kernel__hybrid_cascade_reading, tangled_rope).
narrative_ontology:human_readable(imposition_pathway_kernel__hybrid_cascade_reading, "State-Manufactured Fringe Cascade (Meiji-Era Commitment Displacement)").
narrative_ontology:topic_domain(imposition_pathway_kernel__hybrid_cascade_reading, "historical_sociology/state_formation/commitment_systems").

domain_priors:requires_active_enforcement(imposition_pathway_kernel__hybrid_cascade_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_pathway_kernel__hybrid_cascade_reading, '274e17af-46a9-4023-91e7-9fab567f6687').
narrative_ontology:cs_kernel_codification('274e17af-46a9-4023-91e7-9fab567f6687', implicit).
narrative_ontology:cs_authority_grounding('274e17af-46a9-4023-91e7-9fab567f6687', extraction).
narrative_ontology:cs_interpretation_layer_present('274e17af-46a9-4023-91e7-9fab567f6687').
narrative_ontology:cs_reading_relation('274e17af-46a9-4023-91e7-9fab567f6687', imposition_pathway_kernel__endogenous_climb_reading, influences).
narrative_ontology:cs_reading_relation('274e17af-46a9-4023-91e7-9fab567f6687', imposition_pathway_kernel__exogenous_override_reading, influences).
narrative_ontology:cs_axiom('274e17af-46a9-4023-91e7-9fab567f6687', foundational, state_manufactures_fringe_then_climb_completes_organically).
narrative_ontology:cs_axiom_status(state_manufactures_fringe_then_climb_completes_organically, holdable).
narrative_ontology:cs_axiom_grounding('274e17af-46a9-4023-91e7-9fab567f6687', state_manufactures_fringe_then_climb_completes_organically, empirically_contingent).
narrative_ontology:cs_axiom('274e17af-46a9-4023-91e7-9fab567f6687', foundational, override_and_climb_are_both_structurally_necessary).
narrative_ontology:cs_axiom_status(override_and_climb_are_both_structurally_necessary, holdable).
narrative_ontology:cs_axiom_grounding('274e17af-46a9-4023-91e7-9fab567f6687', override_and_climb_are_both_structurally_necessary, empirically_contingent).
narrative_ontology:cs_reference_frame('274e17af-46a9-4023-91e7-9fab567f6687', feudal_commitment_order_pre_meiji).
narrative_ontology:cs_drift_state('274e17af-46a9-4023-91e7-9fab567f6687', post_sino_japanese_war_1895, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('274e17af-46a9-4023-91e7-9fab567f6687', '').
narrative_ontology:cs_kernel_id(imposition_pathway_kernel__hybrid_cascade_reading, imposition_pathway_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__hybrid_cascade_reading, state_bureaucracy).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__hybrid_cascade_reading, military_officer_corps).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__hybrid_cascade_reading, imperial_institution).
narrative_ontology:constraint_victim(imposition_pathway_kernel__hybrid_cascade_reading, traditional_samurai_class).
narrative_ontology:constraint_victim(imposition_pathway_kernel__hybrid_cascade_reading, domain_loyal_retainers).
narrative_ontology:constraint_victim(imposition_pathway_kernel__hybrid_cascade_reading, rural_population_subject_to_conscription).
narrative_ontology:constraint_vindicates(imposition_pathway_kernel__hybrid_cascade_reading, state_capacity_enables_commitment_displacement).
narrative_ontology:constraint_vindicates(imposition_pathway_kernel__hybrid_cascade_reading, artificial_fringe_becomes_organic_climb_vector).
narrative_ontology:constraint_vindicates(imposition_pathway_kernel__hybrid_cascade_reading, meiji_restoration_as_commitment_cascade).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts and administers the Meiji decrees (haircut edicts, conscription law, status abolition). Captures the new status hierarchy: bureaucratic rank replaces hereditary status, pensions replace stipends, authority flows from imperial institution through bureaucracy. Cannot exit — the bureaucracy IS the new commitment order; leaving means losing structural position entirely.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, state_bureaucracy, agenda_setter,
    institutional, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(imposition_pathway_kernel__hybrid_cascade_reading, state_bureaucracy, beneficiary).

% Enforces conscription and embodies the new national commitment (bushido reconstituted as imperial loyalty). Officer status becomes the new elite marker replacing samurai status. Identity is fused with the imperial military project — exit would require repudiating the self-concept forged through the cascade. The corps administers the artificial fringe (mandatory service) that becomes the climb vector.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, military_officer_corps, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(imposition_pathway_kernel__hybrid_cascade_reading, military_officer_corps, beneficiary).

% The restored emperor is the symbolic anchor of the new commitment order. The cascade vindicates the imperial legitimacy doctrine (vindicated_proposition). Material rent capture is contested: the imperial household gains symbolic capital and some land revenues, but the bureaucracy and military capture the operational rents. Exit is identity_locked — the institution's identity IS the commitment order it anchors.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, imperial_institution, beneficiary,
    institutional, civilizational, identity_locked, national).

% Loses hereditary stipends (converted to government bonds that depreciate), exclusive military status (conscription universalizes the warrior role), and topknot/sword privileges (haircut edict, sword abolition). Their identity is constituted through feudal loyalty and warrior status — the cascade extracts the material basis of that identity and offers no coherent exit. Resistance (Satsuma Rebellion) is crushed by the very conscript army that replaced them.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, traditional_samurai_class, payer,
    powerful, biographical, identity_locked, national).

% Domain (han) abolition (1871) dissolves their feudal bond to daimyo. They become prefectural functionaries or unemployed. Their commitment was to domain and lord; the cascade extracts that commitment and replaces it with imperial bureaucracy. Exit is identity_locked — domain loyalty was a totalizing identity; the new order offers no translation path for that loyalty.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, domain_loyal_retainers, payer,
    organized, biographical, identity_locked, regional).

% Conscription law (1873) extracts bodies and labor for the new national army. Peasant uprisings (e.g., 1873-74 conscription riots) show resistance but exit is trapped — no alternative polity, economic dependency on land, state monopoly on violence. They pay the bodily cost of the cascade's climb phase (the artificial fringe's organic diffusion requires mass participation).
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, rural_population_subject_to_conscription, payer,
    powerless, biographical, trapped, national).

% Analyzes the cascade structure from outside: sees override initiating artificial fringe, climb completing organically. Not subject to the constraint's extraction or coordination. Provides the M-set framework that classifies this as a distinct cell. Exit is analytical — the sociologist can change frameworks without personal cost.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, historical_sociologist, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imposition_pathway_kernel__hybrid_cascade_reading, state_bureaucracy).
narrative_ontology:fixing_cost_class(imposition_pathway_kernel__hybrid_cascade_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a unified national commitment order capable of mobilizing population, resources, and loyalty for modern state defense against colonization — solving the genuine collective-action problem of feudal fragmentation in the face of external threat.
% TRANSFER_FUNCTION: Moves status, material resources (stipends, land revenues, bodily labor), and identity-anchoring commitments from the traditional samurai class, domain retainers, and rural population to the new state bureaucracy, military officer corps, and imperial institution.
% ABSENT_VOICES: The peasantry's pre-existing communal commitments (village mutual aid, folk religion, regional identities) are not represented in the cascade's design — they are overwritten by the national commitment order. Would-be alternative modernizers (e.g., popular rights movement activists) are excluded from the state-manufactured fringe because the fringe is restricted to state employees and military.
% DISAPPEARANCE_RATIONALE: If the hybrid cascade mechanism vanished overnight, the Meiji commitment displacement would not have occurred: no artificial fringe means no climb vector; the feudal commitment order would persist or fragment differently. Japan's modernization trajectory, its war-fighting capacity in 1894-95 and 1904-05, and the entire structure of 20th-century Japanese state-society relations would be unrecognizable.
% FOUNDING_PROBLEM: Feudal Japan's fragmented commitment order (domain loyalties, samurai privilege, no national mobilization capacity) made it vulnerable to colonization by Western powers — the genuine coordination problem was creating a unified national commitment order capable of modern defense.
% FOUNDING_PROBLEM_CORROBORATION: The colonization vulnerability was resolved by the 1890s (unequal treaties revised, victories in Sino-Japanese and Russo-Japanese Wars) — attested by diplomatic historians outside the Meiji state's beneficiaries. The cascade persisted and intensified extraction (bureaucratic expansion, military autonomy, colonial empire) after the founding problem was dead — attested by social historians of the Taisho/Showa periods. No beneficiary of the cascade (bureaucracy, military) attests the problem is dead; they perpetuate the emergency framing.
narrative_ontology:disappearance_verdict(imposition_pathway_kernel__hybrid_cascade_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_pathway_kernel__hybrid_cascade_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_pathway_kernel__hybrid_cascade_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(imposition_pathway_kernel__hybrid_cascade_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_pathway_kernel__hybrid_cascade_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_pathway_kernel__hybrid_cascade_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imposition_pathway_kernel__hybrid_cascade_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imposition_pathway_kernel__hybrid_cascade_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the cascade extracts status, identity, and material resources from the old commitment order (samurai stipends, domain loyalties, bodily autonomy via conscription) and redistributes them to the new state-military-bureaucratic complex. Suppression (0.72) is high because the cascade's initiation requires active enforcement of decrees (haircut edicts, conscription enforcement, status abolition) and the climb phase suppresses alternative commitment pathways by making the new commitments the only route to status and survival. Theater ratio (0.28) is moderate-low: the coordination function (creating a unified national commitment order capable of resisting colonization) is real, but a growing share of enforcement activity serves rent extraction by the new elite rather than the coordination function. Accessibility collapse (0.62) is substantial — once the artificial fringe is established and climb begins, alternative commitment orders (feudal loyalty, traditional status) become structurally inaccessible for most agents. Resistance (0.58) is significant — Satsuma Rebellion (1877), peasant uprisings against conscription, and samurai resistance movements show active contestation, but resistance is ultimately overcome by the cascade's momentum. The claimed type is tangled_rope because the cascade has a genuine coordination function (national unification against external threat) AND asymmetric extraction (new elite captures rents from old order), requiring active enforcement to hold.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter seats (state_bureaucracy, military_officer_corps) experience the cascade as coordination with necessary enforcement — they built the new order and benefit from it. The payer seats (traditional_samurai_class, domain_loyal_retainers, rural_population) experience it as extraction enforced by state violence — their commitments are displaced, their resources captured. The imperial_institution seat is ambiguous: as vindicated proposition it experiences the cascade as legitimation of its authority; if it materially captures rents, it experiences it as beneficiary. The observer seat sees the full hybrid structure: override initiates, climb completes. The engine computes this divergence from the structural data — beneficiaries get low d, payers get high d, the imperial institution's d depends on the omega resolution.
 *
 * DIRECTIONALITY LOGIC:
 *   state_bureaucracy and military_officer_corps are declared beneficiaries — they collect status, pensions, and authority from the new commitment order. Their exit is trapped/identity_locked: they ARE the new order; leaving it means losing their structural position. traditional_samurai_class and domain_loyal_retainers are declared victims — they bear the costs of status abolition, stipend conversion, and identity loss. Their exit is identity_locked: their self-concept is constituted through the old commitments; exit is unthinkable without self-dissolution. rural_population_subject_to_conscription are victims with trapped exit — no identity fusion, but physical coercion (conscription) and economic dependency prevent exit. imperial_institution is a vindicated_proposition (legitimacy doctrine) not a beneficiary — it collects symbolic capital but material rents flow to bureaucracy/military. The derivation chain gives low d to bureaucracy/military (beneficiaries), high d to samurai/retainers/peasants (victims), and the imperial institution's d is unresolved (omega).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was genuine: Meiji Japan faced existential colonization threat and needed a unified national commitment order capable of mobilizing resources for modern defense. The hybrid cascade solved this — but the coordination function (national survival) was achieved by the 1890s, while the extraction function (bureaucratic/military rent capture from the displaced old order) persisted and intensified. The mandate (unified commitment order for national defense) atrophied into a structure that primarily serves the new elite's rent collection. The mandatrophy is resolved in the sense that the original problem is dead (Japan is no longer colonization-vulnerable) but the arrangement persists with intensified extraction — a classic mandatrophy signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'Is the hybrid_cascade_reading a distinct structural mechanism from endogenous_climb and exogenous_override, or a synthesis that merely redescribes their boundary?',
    'M-set framework calibration: if the compressed climb with state-manufactured fringe produces distinct empirical predictions from pure endogenous climb and pure exogenous override, it is a distinct cell; if predictions converge, it is a boundary redesignation.',
    'If distinct, the kernel contains three structurally different readings; if synthesis, the kernel is binary with this reading as the boundary case.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Whether hybrid_cascade is a third M-set cell or a boundary redesignation of the endogenous/exogenous distinction.').

omega_variable(
    artificial_fringe_naturalization,
    'At what point does the state-manufactured fringe (mandatory adoption by bureaucrats/soldiers) become indistinguishable from an organic fringe in the climb dynamic?',
    'Longitudinal tracking of adoption curves: if the mandatory cohort''s descendants show voluntary adoption patterns matching organic fringes within 1-2 generations, the artificial fringe has naturalized; if coercion signatures persist, it remains a distinct mechanism.',
    'If naturalization is rapid and complete, hybrid_cascade converges with endogenous_climb at the macro scale; if persistent, the state''s role in fringe manufacture leaves a structural trace in the commitment system''s topology.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(artificial_fringe_naturalization, empirical, 'The temporal boundary where state-imposed fringe becomes organic climb vector.').

omega_variable(
    imperial_institution_beneficiary_status,
    'Does the imperial institution itself benefit from the commitment cascade, or is it a vindicated proposition (legitimacy doctrine) rather than a rent-collecting actor?',
    'Resource flow analysis: does the imperial household/institution capture material rents from the new commitment order, or only symbolic legitimacy? Compare with state_bureaucracy and military_officer_corps capture.',
    'If the imperial institution is a material beneficiary, it belongs in beneficiaries[]; if only symbolic, it belongs in vindicated_propositions. Affects extraction distribution and directionality for the agenda_setter seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imperial_institution_beneficiary_status, empirical, 'Whether the imperial institution collects rents or only symbolic capital from the cascade.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_pathway_kernel__hybrid_cascade_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t0, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(impo_tr_t5, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(impo_tr_t10, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(impo_tr_t15, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement(impo_tr_t20, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(impo_tr_t25, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 25, 0.28).

% Extraction over time
narrative_ontology:measurement(impo_be_t0, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(impo_be_t5, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(impo_be_t10, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 10, 0.67).
narrative_ontology:measurement(impo_be_t15, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 15, 0.73).
narrative_ontology:measurement(impo_be_t20, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 20, 0.76).
narrative_ontology:measurement(impo_be_t25, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 25, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t0, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(impo_su_t5, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 5, 0.7).
narrative_ontology:measurement(impo_su_t10, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 10, 0.72).
narrative_ontology:measurement(impo_su_t15, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 15, 0.71).
narrative_ontology:measurement(impo_su_t20, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(impo_su_t25, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 25, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_pathway_kernel__hybrid_cascade_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(imposition_pathway_kernel__hybrid_cascade_reading, 0.12).
narrative_ontology:affects_constraint(imposition_pathway_kernel__hybrid_cascade_reading, imposition_pathway_kernel__endogenous_climb_reading).
narrative_ontology:affects_constraint(imposition_pathway_kernel__hybrid_cascade_reading, imposition_pathway_kernel__exogenous_override_reading).

% DUAL FORMULATION NOTE:
% Constraint family: imposition_pathway_kernel with three readings. This reading (hybrid_cascade) structurally influences both siblings: it influences endogenous_climb by showing that some fringes are state-manufactured not organic (changing the fringe-origin taxonomy); it influences exogenous_override by showing that override without climb completion fails (the artificial fringe must naturalize). The kernel decomposition follows ε-invariance: each reading has distinct ε (endogenous ~0.35, exogenous ~0.85, hybrid ~0.78), distinct beneficiary/victim structures, and distinct M-set cells.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(imposition_pathway_kernel__hybrid_cascade_reading, institutional, 0.15).
constraint_indexing:directionality_override(imposition_pathway_kernel__hybrid_cascade_reading, powerful, 0.85).
constraint_indexing:directionality_override(imposition_pathway_kernel__hybrid_cascade_reading, organized, 0.8).
constraint_indexing:directionality_override(imposition_pathway_kernel__hybrid_cascade_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
