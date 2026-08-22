% ============================================================================
% CONSTRAINT STORY: shinbutsu_coexistence_commitment__syncretic_fusion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_syncretic_fusion, []).

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
 *   constraint_id: shinbutsu_coexistence_commitment__syncretic_fusion_reading
 *   human_readable: Honji Suijaku Syncretic Fusion: Kami as Buddhist Manifestations
 *   domain: religious_studies/philosophy_of_religion/japanese_history
 *
 * SUMMARY:
 *   The honji suijaku (original essence, manifest traces) framework unified
 *   kami and Buddhist deities into a single ontology: kami were local,
 *   compassionate manifestations of universal Buddhist truths. This reading
 *   instantiates the syncretic fusion interpretation — a single coherent
 *   cosmic order in which kami and Buddhas occupy complementary
 *   soteriological roles. The fusion was maintained through theological
 *   interpretation, jinguji institutional embodiment, and active suppression
 *   of alternative framings. The constraint benefited the Buddhist clerical
 *   establishment and the jinguji network (which collected dual offerings and
 *   authority) while reorganizing kami devotional practitioners' spiritual
 *   agency toward Buddhist institutional goals. The claim/metric gap is
 *   intentional: this reading is CLAIMED as a tangled rope (genuine
 *   coordination solving the coexistence problem) while the authored metrics
 *   reflect substantially extractive, heavily suppressed operation — the
 *   engine measures whether the fusion's actual function aligns with the
 *   coordination framing or exceeds it.
 *
 * KEY AGENTS:
 *   - Theological Interpretive Elite (Tendai, Shingon scholars): Set doctrinal standard; adjudicate kami-Buddha mappings; enforce consistency through textual authority — institutional power, national scope, high exit barriers through prestige.
 *   - Jinguji Institutional Network: Temple-shrine complexes embodying the fusion; collect dual offerings; exercise ritual oversight over both kami and Buddhist practitioners — institutional power, national organization, constrained exit through property/patronage.
 *   - Kami Devotional Practitioners (villagers, local communities): Venerate kami for practical life benefit; reorganized as participants in universal Buddhist salvation; structurally subordinated to elite interpretation — powerless, local scope, identity-locked exit through community membership.
 *   - Buddhist Clerical Establishment: Gain institutional reach, expanded congregations, interpretive jurisdiction over kami-related questions through the fusion — institutional power, national scope, beneficiary.
 *   - Non-Syncretic Doctrinal Factions (Buddhists and Shinto scholars rejecting fusion): Operate within institutions enforcing the syncretic frame; defend heterodox positions against elite apparatus; structurally suppressed — moderate power, regional scope, constrained exit.
 *   - Court and State Authorities: Observe fusion as enabling unified management of religious life; benefit from coordination but also have authority to alter it — powerful, national scope, analytical seat.
 *   - Meiji Modernization Authorities (19th century): Would later mandate syncretic dissolution; excluded from the framework's real-time authority; represent future foreclosure of this reading — powerful, national scope, excluded from current debate.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.62).
domain_priors:suppression_score(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.71).
domain_priors:theater_ratio(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__syncretic_fusion_reading, "Honji Suijaku Syncretic Fusion: Kami as Buddhist Manifestations").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__syncretic_fusion_reading, "religious_studies/philosophy_of_religion/japanese_history").

domain_priors:requires_active_enforcement(shinbutsu_coexistence_commitment__syncretic_fusion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 'b0d5da24-591e-49f0-a3b5-13067385dd4d').
narrative_ontology:cs_kernel_codification('b0d5da24-591e-49f0-a3b5-13067385dd4d', fixed_text).
narrative_ontology:cs_authority_grounding('b0d5da24-591e-49f0-a3b5-13067385dd4d', lineage).
narrative_ontology:cs_interpretation_layer_present('b0d5da24-591e-49f0-a3b5-13067385dd4d').
narrative_ontology:cs_reading_relation('b0d5da24-591e-49f0-a3b5-13067385dd4d', shinbutsu_coexistence_commitment__domain_partition_reading, forecloses).
narrative_ontology:cs_reading_relation('b0d5da24-591e-49f0-a3b5-13067385dd4d', shinbutsu_coexistence_commitment__incoherent_bundle_reading, coexists_with).
narrative_ontology:cs_axiom('b0d5da24-591e-49f0-a3b5-13067385dd4d', foundational, universal_buddha_kami_manifestation_principle).
narrative_ontology:cs_axiom_status(universal_buddha_kami_manifestation_principle, holdable).
narrative_ontology:cs_axiom_grounding('b0d5da24-591e-49f0-a3b5-13067385dd4d', universal_buddha_kami_manifestation_principle, deontological).
narrative_ontology:cs_axiom('b0d5da24-591e-49f0-a3b5-13067385dd4d', foundational, single_coherent_cosmic_soteriology).
narrative_ontology:cs_axiom_status(single_coherent_cosmic_soteriology, holdable).
narrative_ontology:cs_axiom_grounding('b0d5da24-591e-49f0-a3b5-13067385dd4d', single_coherent_cosmic_soteriology, conventional).
narrative_ontology:cs_reference_frame('b0d5da24-591e-49f0-a3b5-13067385dd4d', unified_honji_suijaku_metaphysics).
narrative_ontology:cs_drift_state('b0d5da24-591e-49f0-a3b5-13067385dd4d', meiji_modernization_imposition, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('b0d5da24-591e-49f0-a3b5-13067385dd4d', '').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, jinguji_institutional_network).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, buddhist_clerical_establishment).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, theological_interpretive_elite).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, kami_devotional_practitioners).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, non_syncretic_doctrinal_factions).
narrative_ontology:constraint_vindicates(shinbutsu_coexistence_commitment__syncretic_fusion_reading, universal_buddhist_truth_doctrine).
narrative_ontology:constraint_vindicates(shinbutsu_coexistence_commitment__syncretic_fusion_reading, kami_salvific_function_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Buddhist scholars, esoteric transmission lineages (especially Tendai and Shingon), and jinguji head priests who author and maintain the honji suijaku ontology. They set the doctrinal standard for what counts as coherent fusion, adjudicate boundary disputes (which kami unify with which Buddhas), and enforce consistency through textual interpretation and institutional authority. The fusion framework legitimates their interpretive power and prestige.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, theological_interpretive_elite, agenda_setter,
    institutional, generational, arbitrage, national).

% Temple-shrine complexes (jinguji) that house both Buddhist altars and kami shrines under one institutional roof. The syncretic fusion justifies their structural arrangement and dual ritual authority. They collect offerings from both devotional communities, exercise ritual oversight, and position themselves as the institutional embodiment of the coherent ontology.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, jinguji_institutional_network, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__syncretic_fusion_reading, jinguji_institutional_network, agenda_setter).

% Local villagers, families, and community groups who venerate specific kami (agricultural deities, household protectors, locality spirits) for practical benefit in life — harvest, health, safety, fertility. The honji suijaku fusion reframes their kami as manifestations of distant Buddhist truths. Their devotion is reorganized as participating in universal Buddhist salvation, whether or not they understand or assent to the metaphysical claim. Their identity as members of the community is fused with participation in the kami cult, making exit from the framework structurally difficult.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, kami_devotional_practitioners, payer,
    powerless, biographical, identity_locked, local).

% Ordained Buddhist clergy outside jinguji who benefit from the fusion through expanded institutional reach into kami-venerating communities, increased patronage and offerings from kami practitioners integrated into Buddhist ritual, and theological expansion that brings kami-related questions into Buddhist doctrinal authority. The fusion multiplies their congregations and their interpretive jurisdiction.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, buddhist_clerical_establishment, beneficiary,
    institutional, generational, constrained, national).

% Practitioners and scholars within both Buddhism and Shinto traditions who see the fusion as incoherent, reductive, or doctrinal contamination. Some Buddhists view kami-fusion as diluting Buddhist soteriology; some kami-centrists see the fusion as subordinating kami to foreign ideology. They must operate within institutions that enforce the syncretic reading, defend their positions against the elite interpretive apparatus, and face institutional suppression of alternatives.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, non_syncretic_doctrinal_factions, payer,
    moderate, generational, constrained, regional).

% Imperial court, state ritual apparatus, and civil administration that interact with both kami shrines and Buddhist temples. They observe the honji suijaku fusion as a framework that allows unified state management of religious life (kami and Buddhas both serve state legitimacy through the single ontological frame). They have authority to alter the constraint through policy, but also benefit from the coordination the fusion provides.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, court_and_state_authorities, observer,
    powerful, generational, analytical, national).

% Late 19th-century state ideology-setters who would later mandate the dissolution of syncretic arrangements in favor of a pure kami-centered Shinto. They are structurally excluded from the framework's authority but would argue that the fusion obscures kami authenticity and serves imported religion at kami's expense. Their exclusion is enforced by the institutional entrenchment of the fusion during the Edo-early Meiji periods.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, meiji_modernization_authorities, excluded,
    powerful, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_coexistence_commitment__syncretic_fusion_reading, jinguji_institutional_network).
narrative_ontology:fixing_cost_class(shinbutsu_coexistence_commitment__syncretic_fusion_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifies kami-veneration and Buddhist practice into a single soteriological framework: local kami become the earthly compassionate manifestations of universal Buddhist truths, allowing kami practitioners to participate in Buddhist salvation without abandoning kami worship. A single interpretive authority (the theological elite) can adjudicate questions about how kami and Buddhas relate, reducing friction between two powerful religious systems in shared institutional space.
% TRANSFER_FUNCTION: Moves interpretive authority from kami-venerating communities to the theological elite and Buddhist clerical hierarchy. Kami practitioners' offerings and devotion are reorganized as contributions to Buddhist institutional strength and the jinguji network's dual authority. The framework transfers legitimacy from decentralized kami cults to centralized institutional Buddhism while maintaining the appearance of honoring kami.
% ABSENT_VOICES: Practitioners within kami communities who view the fusion as intellectually incoherent or as Buddhist appropriation of kami identity are structurally excluded from authority over the framework's content. Scholars and theologians who see the two systems as ontologically separate or as irreconcilable are marginalized within institutional Buddhism. Later Meiji ideologists (19th century) who would dismantle the fusion are not present during the fusion's height and cannot contest it in real time.
% DISAPPEARANCE_RATIONALE: If the honji suijaku fusion and its institutional enforcement vanished, kami practice and Buddhist practice would likely reorganize into more explicit separate spheres with different deities, ritual specialists, and salvation narratives. Jinguji would need to choose whether to operate as Buddhist temples or kami shrines; the theological elite would lose their interpretive monopoly over kami-Buddha relationships; offerings and patronage would redistribute toward non-syncretic institutions. The religious landscape would fragment into clearer sectarian divisions.
% FOUNDING_PROBLEM: Early medieval Japan had two powerful religious systems — indigenous kami-veneration networks and imported Buddhism — in competition for resources, authority, institutional presence, and devotional loyalty. The honji suijaku fusion was constructed to solve the zero-sum conflict by reframing both as manifestations of a single cosmic order, allowing the state, elites, and institutional structures to benefit from both simultaneously without choosing between them.
% FOUNDING_PROBLEM_CORROBORATION: The Buddhist clerical establishment and jinguji leadership attest the founding problem persists and justify the fusion as necessary coordination. Historical scholars and theologians outside benefiting institutions (Maruyama Masao, contemporary Buddhist studies) argue the founding problem was primarily about political power and institutional monopoly rather than genuine metaphysical confusion, and that the fusion solved a political problem while creating doctrinal incoherence. Some kami-centered practitioners and contemporary Shinto scholars attest the fusion never solved the real problem — kami remain subordinated under Buddhist authority even with the metaphysical unity claim.
narrative_ontology:disappearance_verdict(shinbutsu_coexistence_commitment__syncretic_fusion_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_coexistence_commitment__syncretic_fusion_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_coexistence_commitment__syncretic_fusion_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__syncretic_fusion_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_coexistence_commitment__syncretic_fusion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.62 endpoint) because the fusion genuinely coordinates two religious systems (solving a real institutional conflict) but also systematically transfers interpretive authority and offerings from decentralized kami communities to centralized Buddhist institutions and the theological elite. Early extractiveness is lower (0.45) when the fusion is newly instantiated and participatory; it rises as the institutional apparatus of enforcement hardens and alternative interpretations are suppressed. Theater ratio climbs from 0.25 to 0.48 as the theological content becomes increasingly abstract and detached from practitioners' lived kami devotion — by the fusion's maturity, a growing share of ritual activity defends the doctrinal boundary rather than serving practitioners' practical spiritual needs. Suppression requirement parallels extractiveness: enforcement begins at 0.52 (early-phase theological debates) and rises to 0.71 as institutional machinery must actively exclude heterodox interpretations and enforce the syncretic reading against competing framings. The plateau after t=25 indicates the fusion has become institutionally normalized — its suppression is now structural (embedded in training, authority, institutional incentives) rather than active enforcement of novel doctrine. All measurements share a single time grid (required by schema) so the stories of extraction accumulation, theater increase, and suppression hardening are temporally coherent.
 *
 * PERSPECTIVAL GAP:
 *   The theological elite experience the fusion as a breakthrough: two incompatible systems reconciled into coherent metaphysics. Kami devotional practitioners experience it as reinterpretation imposed from above: their local, practical kami reframed as manifestations of distant Buddhist truths. Non-syncretic scholars experience it as incoherent doctrine enforced by institutional power: it satisfies no one theologically but serves everyone institutionally except them. The court experiences it as ideal: religious factionalism solved without choosing between systems. Each seat's directionality derives from this structural asymmetry — beneficiary seats experience coordination; payer/excluded seats experience enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   The theological interpretive elite sits at d ≈ 0.15 (beneficiary): they author the framework, collect prestige and expanded authority, face minimal exit barriers (their power is portable). Jinguji leadership sits at d ≈ 0.20 (beneficiary): they collect dual offerings and institutional legitimacy. The Buddhist clerical establishment sits at d ≈ 0.18 (beneficiary): expanded reach and institutional authority. Kami practitioners sit at d ≈ 0.75 (target): their devotion is reorganized toward elite goals, their alternatives are collapsed (identity-locked to community membership), their exit is structurally trapped. Non-syncretic factions sit at d ≈ 0.65 (target): they must defend heterodox positions against the elite apparatus and face institutional suppression. Court authorities sit at d ≈ 0.50 (symmetric): they benefit from coordination but could alter the framework. These derivations rest on beneficiary/victim declarations plus exit options: practitioners are trapped and identity-locked (high d); elite are powerful with arbitrage options (low d). The directed symmetry between extraction direction and power asymmetry is the story of how a coordination problem becomes a power apparatus.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (two religious systems in zero-sum conflict for state legitimacy and resources) is CONTESTABLY live, not objectively dead. The Buddhist clerical establishment and court attest it remains live: without the fusion, kami and Buddhist temples would still compete destructively. Kami practitioners and heterodox scholars attest the founding problem WAS solved by institutional coexistence (temples can house both) but the fusion was ADDED as a theoretical justification for extractive institutional arrangement. The mismatch (founding_problem_status=contested + disappearance_verdict=world_rearranges) triggers no mandatrophy flag, because the constraint's function is genuinely still performing coordination even as it extracts. The theater_ratio climb from 0.25 to 0.48 suggests that by maturity, theoretical justification labor has increased relative to functional coordination labor — but this is a drift signal, not mandatrophy proper. The constraint does not become a piton (theater ratio would need to exceed 0.65 and extraction would need to vanish). Instead it becomes a more-hardened tangled rope: the coordination and extraction remain coupled, but the enforcement infrastructure has shifted from active debate to institutional embedding.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    syncretic_coherence_vs_institutional_pragmatism,
    'Is the honji suijaku fusion a genuine metaphysical breakthrough that successfully unifies two religious systems into coherent ontology, or is it primarily an institutional pragmatism — a framework that allows both systems to coexist and extract value without requiring theological coherence?',
    'Textual analysis of how well the fusion actually maps specific kami to specific Buddhas across the full corpus of kami veneration; survey of practitioner understanding of the fusion (do kami devotees understand their kami as Buddhist manifestations, or do they treat them as separate?); examination of how often the fusion framework is invoked to resolve actual theological problems vs. invoked to justify institutional arrangements.',
    'If genuine coherence: the constraint is a rope (true coordination solving a real problem). If primarily pragmatic: the constraint is more accurately a snare masquerading as coordination, and the extraction is the primary function. The whole classification could shift from tangled_rope toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(syncretic_coherence_vs_institutional_pragmatism, empirical, 'Whether the fusion framework is metaphysically coherent or institutionally pragmatic cover.').

omega_variable(
    kami_practitioner_actual_subordination,
    'Do kami devotional practitioners experience the honji suijaku framework as intellectually coherent unification, as benign reinterpretation of their practice, or as structural subordination of kami to Buddhist authority?',
    'Ethnographic work documenting how kami practitioners actually understand and narrate the fusion (if at all); historical examination of resistance or re-interpretation by kami communities; analysis of whether syncretic enforcement required active suppression or arose naturally from shared religiosity.',
    'If practitioners genuinely adopt the fusion as coherent: suppression metric should be lower and theater_ratio should be lower. If practitioners resist or reinterpret it: suppression and theater increase, extraction becomes more explicit, classification shifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kami_practitioner_actual_subordination, empirical, 'Whether kami practitioners understand the fusion as unification or subordination.').

omega_variable(
    institutional_dependency_and_identity_lock,
    'How much of the kami practitioners'' identity-locked exit derives from structural economic dependency on the jinguji institution vs. from internalized spiritual identity that has fused with community membership and kami veneration itself?',
    'Historical analysis of community responses to syncretic disruption (Meiji period dissolution records, Edo-period disputes); examination of whether practitioners who left the jinguji network maintained kami devotion in non-institutional forms.',
    'If primarily structural: suppression is engineered; if primarily internalized identity: the community maintains suppression autonomously even after institutional enforcement is removed. This affects long-term trajectory and the constraint''s classification stability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_dependency_and_identity_lock, empirical, 'Degree to which identity-lock is structural dependency vs. internalized identity fusion.').

omega_variable(
    fusion_as_reading_vs_natural_development,
    'Is the honji suijaku fusion a deliberate reading/interpretation imposed by the theological elite, or did it emerge naturally from centuries of lived coexistence and syncretism, with the elite merely codifying what already existed?',
    'Textual archaeology tracing the emergence of honji suijaku terminology and doctrinal formulation; examination of which scholars first articulated the framework; comparison with regions or periods that lacked this explicit formulation but had similar practices.',
    'If imposed from above: the constraint is more extractive and its legitimacy is more contested (omegas about cover stories). If emergent from below: it has more genuine coordination function and beneficiary-like properties for practitioners. The reading-relation to incoherent_bundle_reading shifts: if emergent, bundle_reading loses plausibility; if imposed, bundle_reading gains credibility as alternative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fusion_as_reading_vs_natural_development, empirical, 'Whether the fusion was deliberately constructed by elites or emerged from lived practice.').

omega_variable(
    syncretic_fusion_vs_domain_partition_referent_ambiguity,
    'When measuring extractiveness (ε) for this syncretic fusion reading, the referent is ''the honji suijaku unified framework as the fusion-reading understands it.'' But is ε stable when measured against the alternative domain-partition reading''s ontology, where kami and Buddhas never unify? Does ε for this reading mean something different if the domain-partition framework is substituted as the measuring standpoint?',
    'Formalize what ε measures in each reading: syncretic-fusion-reading measures extraction inherent in the unified-framework; domain-partition-reading measures extraction in the separation-framework. If the two frameworks'' ε values are substantially different (e.g., 0.62 here vs. 0.35 for partition-reading), then the two readings are genuinely different constraints (per ε-invariance principle), not two measurements of the same constraint.',
    'If ε is reading-invariant: both readings measure the same underlying constraint from different theoretical perspectives. If ε diverges: syncretic-fusion and domain-partition are two different constraints in the same kernel, each with its own structural properties. This is the kernel-reading ε-invariance test.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(syncretic_fusion_vs_domain_partition_referent_ambiguity, conceptual, 'Whether ε for syncretic fusion is reading-invariant or whether the two readings measure structurally different constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(shin_tr_t0, projected).
narrative_ontology:measurement(shin_tr_t5, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(shin_tr_t5, observed).
narrative_ontology:measurement(shin_tr_t10, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement_basis(shin_tr_t10, observed).
narrative_ontology:measurement(shin_tr_t15, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 15, 0.43).
narrative_ontology:measurement_basis(shin_tr_t15, observed).
narrative_ontology:measurement(shin_tr_t20, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 20, 0.47).
narrative_ontology:measurement_basis(shin_tr_t20, observed).
narrative_ontology:measurement(shin_tr_t25, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 25, 0.48).
narrative_ontology:measurement_basis(shin_tr_t25, observed).
narrative_ontology:measurement(shin_tr_t30, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 30, 0.49).
narrative_ontology:measurement_basis(shin_tr_t30, observed).
narrative_ontology:measurement(shin_tr_t40, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 40, 0.48).
narrative_ontology:measurement_basis(shin_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(shin_be_t0, projected).
narrative_ontology:measurement(shin_be_t5, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 5, 0.51).
narrative_ontology:measurement_basis(shin_be_t5, observed).
narrative_ontology:measurement(shin_be_t10, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement_basis(shin_be_t10, observed).
narrative_ontology:measurement(shin_be_t15, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement_basis(shin_be_t15, observed).
narrative_ontology:measurement(shin_be_t20, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement_basis(shin_be_t20, observed).
narrative_ontology:measurement(shin_be_t25, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 25, 0.63).
narrative_ontology:measurement_basis(shin_be_t25, observed).
narrative_ontology:measurement(shin_be_t30, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement_basis(shin_be_t30, observed).
narrative_ontology:measurement(shin_be_t40, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(shin_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(shin_su_t0, projected).
narrative_ontology:measurement(shin_su_t5, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement_basis(shin_su_t5, observed).
narrative_ontology:measurement(shin_su_t10, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement_basis(shin_su_t10, observed).
narrative_ontology:measurement(shin_su_t15, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 15, 0.67).
narrative_ontology:measurement_basis(shin_su_t15, observed).
narrative_ontology:measurement(shin_su_t20, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(shin_su_t20, observed).
narrative_ontology:measurement(shin_su_t25, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(shin_su_t25, observed).
narrative_ontology:measurement(shin_su_t30, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement_basis(shin_su_t30, observed).
narrative_ontology:measurement(shin_su_t40, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(shin_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_coexistence_commitment__syncretic_fusion_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.12).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinbutsu_coexistence_commitment__domain_partition_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinbutsu_coexistence_commitment__incoherent_bundle_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of the shinbutsu_coexistence_commitment kernel family. The kernel represents the persisting arrangement of kami and Buddhist religious systems under a single institutional apparatus (temples, shrines, clerical authority). Three structurally distinct constraints emerge from different readings: (1) SYNCRETIC_FUSION_READING (this file): asserts single coherent ontology via honji suijaku; (2) DOMAIN_PARTITION_READING: asserts separate ontological domains without unification; (3) INCOHERENT_BUNDLE_READING: asserts no coherent framework, only institutional pragmatism and enforced ambiguity. Each reading has distinct beneficiary structures, victim sets, and classifications. The three stories are linked via network.affects_constraints: syncretic_fusion influences the other two by establishing a coherent baseline against which partition and incoherent readings define themselves. All three instantiate the same kernel but instantiate different constraints, following the ε-invariance principle (OQ-26, DP-001): when a kernel can be read via multiple frameworks that yield substantially different ε values and structural data, decompose into separate constraint stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(shinbutsu_coexistence_commitment__syncretic_fusion_reading, powerless, 0.75).
constraint_indexing:directionality_override(shinbutsu_coexistence_commitment__syncretic_fusion_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
