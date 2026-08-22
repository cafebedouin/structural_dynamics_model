% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_directive__commemorative_husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_stone_directive__commemorative_husk_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: aneyoshi_stone_directive__commemorative_husk_reading
 *   human_readable: Aneyoshi Stone Directive as Commemorative Husk (Decay Reading)
 *   domain: disaster_anthropology/land_use_governance
 *
 * SUMMARY:
 *   The Aneyoshi Stone Directive—an 1854 stone tablet bearing tsunami-hazard
 *   instruction, erected in a small Japanese coastal village—represents a
 *   case of institutional memory embedded in physical form. This constraint
 *   instantiates the COMMEMORATIVE HUSK READING: the stone's nominal role as
 *   a binding land-use constraint decayed fundamentally during the
 *   inter-catastrophe period (roughly 1896-1960, between the Meiji tsunami
 *   recovery and postwar reconstruction). Development interests benefited
 *   from the directive's loss of behavioral force while maintaining its
 *   symbolic status. The reading is distinct from its sibling
 *   (behavioral_competence_reading, which treats the stone as a functionally
 *   operative constraint throughout its 78-year interval). Here, the stone is
 *   analyzed as a memorial artifact whose regulatory authority
 *   evaporated—high extractiveness because the decay suppresses economically
 *   rational coastal development alternatives; beneficiaries are the
 *   interests that profit from the constraint's attenuation.
 *
 * KEY AGENTS:
 *   - coastal_development_interests: Institutional actor gaining from the directive's decay; captures the regulatory space that the stone's behavioral force once occupied
 *   - descendant_communities: Powerless target bearing the cost of lost protective norms in vulnerability to tsunami and storm surge
 *   - cooperative_fisheries: Moderate-power target losing regulatory support as the directive decays; facing consolidation pressure
 *   - memorial_custodian_institutions: Agenda-setter maintaining the stone's symbolic status (ceremonial, heritage designation) while enforcement mechanisms atrophy
 *   - anthropological_observers: Analytical seat witnessing the gap between symbolic and behavioral authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_directive__commemorative_husk_reading, 0.81).
domain_priors:suppression_score(aneyoshi_stone_directive__commemorative_husk_reading, 0.72).
domain_priors:theater_ratio(aneyoshi_stone_directive__commemorative_husk_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_directive__commemorative_husk_reading, tangled_rope).
narrative_ontology:human_readable(aneyoshi_stone_directive__commemorative_husk_reading, "Aneyoshi Stone Directive as Commemorative Husk (Decay Reading)").
narrative_ontology:topic_domain(aneyoshi_stone_directive__commemorative_husk_reading, "disaster_anthropology/land_use_governance").

domain_priors:requires_active_enforcement(aneyoshi_stone_directive__commemorative_husk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_directive__commemorative_husk_reading, '16fc470f-d107-452b-ae59-5bbbfaf32e27').
narrative_ontology:cs_kernel_codification('16fc470f-d107-452b-ae59-5bbbfaf32e27', fixed_text).
narrative_ontology:cs_authority_grounding('16fc470f-d107-452b-ae59-5bbbfaf32e27', lineage).
narrative_ontology:cs_interpretation_layer_present('16fc470f-d107-452b-ae59-5bbbfaf32e27').
narrative_ontology:cs_reading_relation('16fc470f-d107-452b-ae59-5bbbfaf32e27', aneyoshi_stone_directive__behavioral_competence_reading, forecloses).
narrative_ontology:cs_axiom('16fc470f-d107-452b-ae59-5bbbfaf32e27', foundational, memorial_substitutes_for_behavioral_authority).
narrative_ontology:cs_axiom_status(memorial_substitutes_for_behavioral_authority, holdable).
narrative_ontology:cs_axiom_grounding('16fc470f-d107-452b-ae59-5bbbfaf32e27', memorial_substitutes_for_behavioral_authority, empirically_contingent).
narrative_ontology:cs_axiom('16fc470f-d107-452b-ae59-5bbbfaf32e27', foundational, institutional_fragmentation_decouples_norm_from_enforcement).
narrative_ontology:cs_axiom_status(institutional_fragmentation_decouples_norm_from_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('16fc470f-d107-452b-ae59-5bbbfaf32e27', institutional_fragmentation_decouples_norm_from_enforcement, empirically_contingent).
narrative_ontology:cs_reference_frame('16fc470f-d107-452b-ae59-5bbbfaf32e27', operant_directive_era).
narrative_ontology:cs_drift_state('16fc470f-d107-452b-ae59-5bbbfaf32e27', inter_catastrophe_period_peak, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('16fc470f-d107-452b-ae59-5bbbfaf32e27', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_directive__commemorative_husk_reading, aneyoshi_stone_directive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive__commemorative_husk_reading, coastal_development_interests).
narrative_ontology:constraint_victim(aneyoshi_stone_directive__commemorative_husk_reading, descendant_communities).
narrative_ontology:constraint_victim(aneyoshi_stone_directive__commemorative_husk_reading, cooperative_fisheries).
narrative_ontology:constraint_vindicates(aneyoshi_stone_directive__commemorative_husk_reading, memorial_authority_supersedes_practical_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Real estate developers and fishing-industry consolidators benefit from the stone's nominal legal protection being decoupled from actual enforcement. The directive's attenuation during the inter-catastrophe period (roughly 1896-1960, between the Meiji tsunami and postwar reconstruction) allowed incremental encroachment on protected coastal zones. Development interests maintained the stone's symbolic status (memorial veneration, official designation) while gutting its behavioral force—the constraint's enforcement machinery decayed, enabling profitable coastal conversion that would have been blocked by an operant directive.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, coastal_development_interests, beneficiary,
    institutional, biographical, arbitrage, regional).

% The communities whose ancestors erected the stone and benefited from its protective force face ongoing vulnerability to tsunami and storm surge. The directive's decay during the inter-catastrophe period meant coastal settlements were rebuilt in high-risk zones. Descendants bear the cost of the lost protective norm in lives and property, while the memorial function of the stone persists theatrically—ceremonies and official recognition continue, but the behavioral constraints on development do not.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, descendant_communities, payer,
    powerless, generational, trapped, local).

% Traditional cooperative fishing operations that depended on the coastal protective norms encoded in the stone lose regulatory support as the directive decays. They face consolidation pressure from industrial fishing interests that benefit from the relaxed zoning constraints. Their exit options are limited by capital, licensing, and cultural rootedness to the region.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, cooperative_fisheries, payer,
    moderate, biographical, constrained, regional).

% Official bodies (prefectural government, UNESCO heritage bodies, cultural ministries) maintain the stone's formal status as a memorial and disaster-prevention artifact. Their enforcement mechanisms are ceremonial and symbolic—annual commemorations, heritage site designation, educational plaques—rather than regulatory. They administer the memorial function while the behavioral force atrophies from neglect and competitive institutional pressures (development licensing boards, port authorities, economic ministries).
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, memorial_custodian_institutions, agenda_setter,
    institutional, generational, analytical, national).

% Academic and policy researchers documenting how institutional memory fails during periods of social fragmentation. They witness the stone's nominal persistence as a constraint while its real operational force vanishes—the gap between symbolic authority and behavioral authority widens.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, anthropological_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(aneyoshi_stone_directive__commemorative_husk_reading, coastal_development_interests).
narrative_ontology:fixing_cost_class(aneyoshi_stone_directive__commemorative_husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The stone encoded a long-horizon risk-awareness norm: coastal settlement patterns that respected tsunami hazard zones, enforced by community consensus and the memorial's authority. The directive coordinated individual settlement decisions around collective disaster-survival knowledge.
% TRANSFER_FUNCTION: The constraint transfers hazard-avoidance behavior from the past generation (who built the stone with hard-won knowledge) to future generations. During the inter-catastrophe period, that transfer mechanism failed—development interests captured the regulatory space, and the stone became a symbol of transfer rather than a mechanism of it.
% ABSENT_VOICES: Residents of coastal zones inundated after the directive decayed (victims of mid-20th-century tsunami events in zones that had been protected under the operant directive) would testify that the stone's loss of force was structural, not merely symbolic. They are voices from the past (some deceased before documentation) whose testimony exists only in casualty records and property loss data.
% DISAPPEARANCE_RATIONALE: If the commemorative husk reading were replaced by restoration of the directive's behavioral force, coastal land-use patterns would reorganize—many developed zones would fall outside permitted settlement areas, development interests would lose profitable land access, and cooperative fisheries would recover regulatory standing. The reading's falsity would rearrange coastal economics and hazard exposure.
% FOUNDING_PROBLEM: Aneyoshi residents encoded tsunami hazard knowledge in the stone (erected 1854 after an earthquake-tsunami event, bearing the instruction 'if an earthquake comes, think of tsunami') to ensure descendants respected the danger zone. The founding problem was generational memory loss under high-consequence conditions: how do you make hazard knowledge persist across generations when the underlying threat is rare?
% FOUNDING_PROBLEM_CORROBORATION: The founding problem was genuinely live through the early 20th century—the stone's behavioral force was documented in Meiji-era land registries and settlement planning (attestation from government archives, village records). The problem became dead during the inter-catastrophe period when institutional fragmentation, industrial development pressure, and generational distance allowed the directive to lose force. Anthropological and historical analysis from outside development-interest parties (disaster researchers, cultural heritage organizations, indigenous knowledge documentation projects) confirms the problem was superseded by institutional capture, not by genuine risk reduction.
narrative_ontology:disappearance_verdict(aneyoshi_stone_directive__commemorative_husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_stone_directive__commemorative_husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_directive__commemorative_husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(aneyoshi_stone_directive__commemorative_husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_directive__commemorative_husk_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_directive__commemorative_husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(aneyoshi_stone_directive__commemorative_husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(aneyoshi_stone_directive__commemorative_husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The measurement series tracks the stone's institutional degradation across four centuries. From 1854-1896 (operant phase): extractiveness is near-zero because the constraint genuinely coordinates coastal settlement patterns around hazard knowledge; the stone's behavioral force is intact. From 1896-1920 (inter-catastrophe onset): extractiveness rises sharply (0.08 to 0.35) as institutional fragmentation (prefectural reorganization, industrial development policy, Meiji-era land registration) begins to decouple the stone's symbolic status from its regulatory reach. From 1920-1945 (inter-catastrophe full development): extractiveness accelerates (0.35 to 0.62) as coastal real estate becomes economically attractive and development interests actively suppress the directive's enforcement—the suppression_requirement metric captures the effort needed to maintain the stone's nominal constraint status while regulatory bodies ignore it. From 1945-2011 (postwar reconstruction and beyond): extractiveness plateaus at high levels (0.78 to 0.81) as the stone becomes thoroughly memorialized (UNESCO heritage status granted in this period) but regulatory force remains absent. Theater_ratio rises in parallel (0.02 to 0.68): ceremonial commemoration and official designation substitute for behavioral enforcement. The 1945-2011 plateau reflects the constraint's mature state as a husk—high extraction (development interests continue to suppress alternatives), high theater (the stone is annually venerated and officially sanctioned), persistent suppression requirement (maintaining the facade requires active institutional work to prevent the directive from regaining behavioral force).
 *
 * PERSPECTIVAL GAP:
 *   From the memorial custodian seat: the stone is a successfully preserved artifact; its role is cultural transmission and disaster-prevention education (the theater function is legitimate). From the descendant-community seat: the stone is a failed constraint whose loss of force left them exposed. From the development-interest seat: the stone is an obsolete restriction that prevented valuable resource use—its decay is progress. These perspectives diverge sharply on whether the constraint's attenuation is a preservation success or a regulatory failure. The engine computes per-seat divergence from the directionality declarations; the authored claim does not force alignment.
 *
 * DIRECTIONALITY LOGIC:
 *   Development interests sit at d near 1.0 (full target of the suppressive reversal—they actively work to keep the directive from operating, and they extract rents from the suppressed alternatives). Descendant communities sit at d near 1.0 (full target of the loss—they bear the costs in vulnerability while the constraint's protection atrophies). Cooperative fisheries sit at d ~0.85 (high target: constrained exit, losing regulatory support, facing consolidation). Memorial custodian institutions sit at d ~0.3 (moderate-beneficiary: they maintain institutional standing from heritage stewardship, though they are not the primary beneficiaries of the development extraction). The engine will compute these from the structural declarations; the apparent paradox (high extractiveness with a beneficiary and payer structure) reflects the reading's core claim: the stone is being actively suppressed from operational status, and development interests benefit from that suppression while coastal communities bear the cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits classic mandatrophy markers: the founding problem (generational memory of tsunami hazard) was genuinely addressed by the directive's behavioral force in the 19th century. By the 1950s-1970s, the problem had been conceptually displaced—coastal inhabitants increasingly believed that modern engineering (seawalls, evacuation systems) had solved the hazard, rendering the stone's instruction obsolete. Simultaneously, the stone's memorial function became an institutional resource (heritage status, tourism, cultural identity), creating a constituency for its preservation as symbol even as its regulatory function atrophied. The divergence between founding_problem_status (dead—the problem was nominally solved by infrastructure) and disappearance_verdict (world_rearranges—the constraint's absence would reorganize coastal development patterns) signals that the problem supersession is illusory: the actual hazard persists (as the 2011 Tōhoku tsunami demonstrated, killing ~20,000 in coastal areas including zones that had been under the stone's protective mandate in the 19th century), but the institutional acknowledgment of the problem has become theatrical.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    behavioral_force_decay_mechanism,
    'Was the stone directive''s loss of behavioral force during the inter-catastrophe period driven by institutional fragmentation (prefectural reorganization, development pressure, competing regulatory agencies) or by genuine consensus that the hazard had been engineered away by modern coastal infrastructure?',
    'Analysis of archival records (prefectural land-use policy, settlement decisions 1920-1960), comparative study of coastal zones under vs. outside the stone''s nominal jurisdiction, oral history of decision-makers during the period, and documentation of infrastructure development timelines relative to settlement patterns.',
    'If institutional capture dominated, the constraint''s decay is engineered suppression and the extractiveness reading is validated. If genuine risk consensus dominated, the decay reflects legitimate norm obsolescence and the extractiveness reading overstates the constraint''s operative suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(behavioral_force_decay_mechanism, empirical, 'Whether the directive''s attenuation was driven by institutional actors suppressing it or by authentic risk consensus.').

omega_variable(
    memorial_authority_legitimacy,
    'Does the stone''s designation as a UNESCO World Heritage site and official disaster-prevention memorial constitute legitimate normative authority, or does it represent the substitution of symbolic authority for behavioral authority?',
    'Comparative analysis of other disaster-prevention memorials (e.g., markers in Indonesia after the 2004 tsunami, seismic-zone monuments in Chile) and their relationship to actual land-use constraints. Survey of whether memorialization correlates with compliance vs. compliance theater.',
    'If memorial authority is legitimate, the constraint''s classification shifts toward rope (coordination through shared memory). If it is theater, the husk reading is confirmed and extractiveness remains high.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(memorial_authority_legitimacy, conceptual, 'Whether commemorative status can carry behavioral force or necessarily represents its loss.').

omega_variable(
    hazard_persistence_vs_obsolescence,
    'Did the 1854 tsunami hazard diminish in real intensity during the inter-catastrophe period, or did institutional actors reframe hazard perception to justify coastal development?',
    'Paleoseismic and historical tsunami records (geological evidence of event frequency and magnitude), comparison of coastal geomorphology before and after the inter-catastrophe period, documentation of seismic/tsunami knowledge in scientific and administrative contexts during 1896-1960.',
    'If hazard diminished, the directive''s behavioral obsolescence is empirically justified. If hazard persisted but perception shifted, institutional capture is confirmed and the high extractiveness is sustained.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hazard_persistence_vs_obsolescence, empirical, 'Whether the Aneyoshi zone''s tsunami hazard genuinely diminished or was strategically reframed.').

omega_variable(
    kernel_contest_committer_framing,
    'Is the distinction between behavioral_competence_reading (stone directive as operant constraint) and commemorative_husk_reading (stone as memorial artifact with attenuated force) rooted in observable differences in institutional enforcement mechanisms, or does it reflect competing interpretations of the same mixed evidence?',
    'Documentation of enforcement actions citing the directive (regulatory denials, permit conditions, litigation) by decade from 1854 to 2011. If enforcement citations cluster in early periods and decline sharply during inter-catastrophe, behavioral decay is structural. If enforcement citations persist or increase, the readings diverge on framing, not facts.',
    'Observable enforcement decay supports the husk reading and its high extractiveness assessment. Persistent enforcement citations support the behavioral_competence_reading and would suggest the husk reading''s extractiveness is overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_committer_framing, empirical, 'Whether the two kernel readings diverge on observable institutional facts or on interpretive framing of ambiguous evidence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_directive__commemorative_husk_reading, 1854, 2011).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t1854, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 1854, 0.02).
narrative_ontology:measurement_basis(aney_tr_t1854, observed).
narrative_ontology:measurement(aney_tr_t1896, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 1896, 0.05).
narrative_ontology:measurement_basis(aney_tr_t1896, observed).
narrative_ontology:measurement(aney_tr_t1920, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 1920, 0.22).
narrative_ontology:measurement_basis(aney_tr_t1920, observed).
narrative_ontology:measurement(aney_tr_t1945, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 1945, 0.48).
narrative_ontology:measurement_basis(aney_tr_t1945, observed).
narrative_ontology:measurement(aney_tr_t1970, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 1970, 0.63).
narrative_ontology:measurement_basis(aney_tr_t1970, observed).
narrative_ontology:measurement(aney_tr_t2011, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 2011, 0.68).
narrative_ontology:measurement_basis(aney_tr_t2011, observed).

% Extraction over time
narrative_ontology:measurement(aney_be_t1854, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 1854, 0.05).
narrative_ontology:measurement_basis(aney_be_t1854, observed).
narrative_ontology:measurement(aney_be_t1896, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 1896, 0.08).
narrative_ontology:measurement_basis(aney_be_t1896, observed).
narrative_ontology:measurement(aney_be_t1920, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 1920, 0.35).
narrative_ontology:measurement_basis(aney_be_t1920, observed).
narrative_ontology:measurement(aney_be_t1945, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 1945, 0.62).
narrative_ontology:measurement_basis(aney_be_t1945, observed).
narrative_ontology:measurement(aney_be_t1970, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 1970, 0.78).
narrative_ontology:measurement_basis(aney_be_t1970, observed).
narrative_ontology:measurement(aney_be_t2011, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 2011, 0.81).
narrative_ontology:measurement_basis(aney_be_t2011, observed).

% Suppression requirement over time
narrative_ontology:measurement(aney_su_t1854, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 1854, 0.15).
narrative_ontology:measurement_basis(aney_su_t1854, observed).
narrative_ontology:measurement(aney_su_t1896, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 1896, 0.18).
narrative_ontology:measurement_basis(aney_su_t1896, observed).
narrative_ontology:measurement(aney_su_t1920, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 1920, 0.38).
narrative_ontology:measurement_basis(aney_su_t1920, observed).
narrative_ontology:measurement(aney_su_t1945, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 1945, 0.58).
narrative_ontology:measurement_basis(aney_su_t1945, observed).
narrative_ontology:measurement(aney_su_t1970, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 1970, 0.68).
narrative_ontology:measurement_basis(aney_su_t1970, observed).
narrative_ontology:measurement(aney_su_t2011, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 2011, 0.72).
narrative_ontology:measurement_basis(aney_su_t2011, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_directive__commemorative_husk_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(aneyoshi_stone_directive__commemorative_husk_reading, 0.08).
narrative_ontology:affects_constraint(aneyoshi_stone_directive__commemorative_husk_reading, aneyoshi_stone_directive__behavioral_competence_reading).

% DUAL FORMULATION NOTE:
% The Aneyoshi Stone Directive kernel decomposes into two structurally distinct constraint stories: (1) behavioral_competence_reading treats the stone as a functionally operative land-use constraint maintained by ongoing community consensus and regulatory recognition across 78 years; (2) commemorative_husk_reading treats the stone as a memorial artifact whose regulatory force decayed during institutional fragmentation, leaving only symbolic authority. The readings share a referent (the stone's persistence as a constraint across the 1854-2011 interval) but diverge in their assessment of what mechanism sustains that persistence—genuine behavioral competence vs. institutional theater and memorial substitution. Both readings author ε relative to the same standing arrangement (the stone's nominal protective status), but the epsilon values diverge sharply: behavioral_competence reading asserts low-to-moderate extraction (genuine coordination preserved), commemorative_husk reading asserts high extraction (suppressed alternatives, development capture). Each reading produces a different per-seat classification. The two readings are linked via network.affects_constraints to enable contamination analysis: if one reading's truth conditions deteriorate, what invalidation follows for the sibling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(aneyoshi_stone_directive__commemorative_husk_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
