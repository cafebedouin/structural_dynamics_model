% ============================================================================
% CONSTRAINT STORY: shinbutsu_coexistence_commitment__domain_partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_coexistence_commitment__domain_partition_reading, []).

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
 *   constraint_id: shinbutsu_coexistence_commitment__domain_partition_reading
 *   human_readable: Kami-Buddha Domain Partition: Separate Existential Spheres
 *   domain: religious/philosophical/institutional
 *
 * SUMMARY:
 *   The domain partition reading frames Kami and Buddhist deities as
 *   governing orthogonal existential domains: kami operate in the sphere of
 *   life, purity, fertility, harvest, and worldly flourishing; Buddhas
 *   operate in the sphere of death, karma, salvation, and the transcendent
 *   path. This reading does not attempt to unify the two systems
 *   theologically (as honji suijaku does) but treats their coexistence as
 *   functional and sustainable precisely because they do not compete. Under
 *   this reading, the partition is a CE commitment that grounds its
 *   legitimacy in popular practice, institutional precedent, and imperial
 *   sanction—not in explicit doctrinal reconciliation. The constraint is the
 *   shared understanding that these domains are separate and that violating
 *   the boundary creates incoherence.
 *
 * KEY AGENTS:
 *   - Buddhist institutional clergy: maintain salvific and funerary monopoly; benefit from ritual exclusivity
 *   - Kami shrine operators: maintain life-blessing and purity monopoly; benefit from complementary jurisdiction
 *   - Imperial court: draws dual legitimacy from both systems; enforces boundary through state authority
 *   - Popular practitioners: benefit from pragmatic access to both systems without doctrinal burden
 *   - Scholastic theologians: excluded from governing authority; would unify or separate the systems
 *   - Later modernizers: would dissolve or reorganize the partition under rationalization pressure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__domain_partition_reading, 0.38).
domain_priors:suppression_score(shinbutsu_coexistence_commitment__domain_partition_reading, 0.42).
domain_priors:theater_ratio(shinbutsu_coexistence_commitment__domain_partition_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__domain_partition_reading, rope).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__domain_partition_reading, "Kami-Buddha Domain Partition: Separate Existential Spheres").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__domain_partition_reading, "religious/philosophical/institutional").

domain_priors:requires_active_enforcement(shinbutsu_coexistence_commitment__domain_partition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__domain_partition_reading, '60b2dd81-32e5-48c3-ac4e-439d4fb63927').
narrative_ontology:cs_kernel_codification('60b2dd81-32e5-48c3-ac4e-439d4fb63927', implicit).
narrative_ontology:cs_authority_grounding('60b2dd81-32e5-48c3-ac4e-439d4fb63927', practice).
narrative_ontology:cs_interpretation_layer_present('60b2dd81-32e5-48c3-ac4e-439d4fb63927').
narrative_ontology:cs_reading_relation('60b2dd81-32e5-48c3-ac4e-439d4fb63927', shinbutsu_coexistence_commitment__syncretic_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('60b2dd81-32e5-48c3-ac4e-439d4fb63927', shinbutsu_coexistence_commitment__incoherent_bundle_reading, coexists_with).
narrative_ontology:cs_axiom('60b2dd81-32e5-48c3-ac4e-439d4fb63927', foundational, orthogonal_existential_domains).
narrative_ontology:cs_axiom_status(orthogonal_existential_domains, holdable).
narrative_ontology:cs_axiom_grounding('60b2dd81-32e5-48c3-ac4e-439d4fb63927', orthogonal_existential_domains, conventional).
narrative_ontology:cs_axiom('60b2dd81-32e5-48c3-ac4e-439d4fb63927', foundational, no_unifying_theology_required).
narrative_ontology:cs_axiom_status(no_unifying_theology_required, holdable).
narrative_ontology:cs_axiom_grounding('60b2dd81-32e5-48c3-ac4e-439d4fb63927', no_unifying_theology_required, conventional).
narrative_ontology:cs_reference_frame('60b2dd81-32e5-48c3-ac4e-439d4fb63927', functional_domain_separation).
narrative_ontology:cs_drift_state('60b2dd81-32e5-48c3-ac4e-439d4fb63927', meiji_modernization_pressure, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('60b2dd81-32e5-48c3-ac4e-439d4fb63927', '').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__domain_partition_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, buddhist_institutional_clergy).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, kami_shrine_operators).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, imperial_court).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, popular_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Buddhist temples and monasteries maintain jurisdiction over death, afterlife, and salvific ritual across all social classes. Under domain partition, Buddhist clergy administer funerary rites, memorial services, and the entire machinery of karmic salvation without doctrinal challenge from kami-worship. This domain monopoly is institutionally protected and revenue-generating (temple endowments, funeral fees). They enforce the boundary by asserting kami operate only in worldly domains outside their purview.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, buddhist_institutional_clergy, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__domain_partition_reading, buddhist_institutional_clergy, agenda_setter).

% Shinto shrine priests and shrine associations maintain jurisdiction over life, fertility, purity, harvest, and worldly prosperity without doctrinal challenge from Buddhist soteriological claims. Under domain partition, kami-worship provides the exclusive ritual path for agricultural blessing, childbirth protection, and renewal of life-force. They enforce the boundary by asserting kami are ontologically distinct from Buddhist deities and operate in a non-salvific domain.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, kami_shrine_operators, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__domain_partition_reading, kami_shrine_operators, agenda_setter).

% The imperial court and state apparatus benefit from dual-system legitimacy: imperial Shinto rites (via kami worship) establish the emperor as the ritual guarantor of agrarian and dynastic continuity, while Buddhist institutional support and royal patronage cement court authority as the protector of universal salvation and Buddhist teachings. Domain partition allows the court to draw legitimacy from both systems without mediating between competing theological claims. The court enforces the partition by regulating which institutions handle which rituals and by sanctioning both systems' authority within their respective domains.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, imperial_court, agenda_setter,
    powerful, generational, mobile, national).

% Commoners, farmers, and urban dwellers participate in both kami worship (for life concerns, harvest, childbirth) and Buddhist practice (for funerary rites, memorial services, personal karma) without doctrinal confusion because the domain partition makes the two systems' functions orthogonal—no theological reconciliation is demanded. They benefit from access to both systems' resources and ritualists without having to adopt a unified theological framework. Exit would mean abandoning access to either domain's protective and salvific functions.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, popular_practitioners, beneficiary,
    powerless, biographical, constrained, regional).

% Buddhist and Shinto intellectuals who seek ontological unification or doctrinal consistency (the syncretic-fusion reading) are structurally excluded from authoritative voice under domain partition. Their arguments for honji suijaku (kami as Buddhist manifestations) or other unifying frames are treated as sectarian scholasticism, not as governing principle. They would object to domain partition as ad-hoc and theologically impoverished, but institutional power lies with practitioners and administrators who profit from the partition.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, scholastic_theologians, excluded,
    moderate, biographical, constrained, national).

% Later political actors seeking national coherence and Western-style rationalization would find the domain partition incoherent and would push for explicit unification, subordination, or separation (shinbutsu bunri). They are excluded from the domain partition's authority structure but would eventually reshape it through state power.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, meiji_modernizers, excluded,
    powerful, biographical, mobile, national).

% Individual believers who navigate both systems pragmatically, attending shrine festivals for worldly blessing and Buddhist temples for funeral rites, without systematic theological framework. They perceive the partition as natural and functional, not as a constraint to be analyzed, and their practical participation reinforces the partition's authority.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, lay_believers, observer,
    powerless, biographical, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_coexistence_commitment__domain_partition_reading, diffuse).
narrative_ontology:fixing_cost_class(shinbutsu_coexistence_commitment__domain_partition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates access to dual religious resources (kami-based life-blessing and Buddhist salvific machinery) across a population without requiring explicit doctrinal reconciliation between incommensurable traditions. Practitioners draw on whichever system addresses their immediate concern (harvest prosperity from kami, death preparation from Buddhist clergy) without facing burden of theological unification.
% TRANSFER_FUNCTION: Moves authority and revenue to both institutional systems: Buddhist temples receive funeral fees, memorial endowments, and scholarly patronage for managing the soteriological domain; shrines receive agricultural offerings, fertility-protection donations, and life-blessing patronage. Imperial court transfers legitimacy to both systems in exchange for their sanctioning of imperial authority.
% ABSENT_VOICES: Scholastic theologians seeking doctrinal unification (honji suijaku advocates) and later modernizers seeking either explicit merger or complete separation are excluded from the partition's governing authority. They would argue the partition is theoretically incoherent and institutionally unstable, but institutional power lies with practitioners and administrators who profit from the partition's functional stability.
% DISAPPEARANCE_RATIONALE: If the domain partition vanished—if kami and Buddhas were forced into explicit theological relationship or if one system's exclusive claim to its domain were abandoned—religious practice and institutional authority would reorganize dramatically. Popular practitioners would need to choose or unify the systems; institutional revenues would shift; the court's dual legitimacy would require regrounding; the entire organizational structure of Japanese religious life would change.
% FOUNDING_PROBLEM: Two major religious traditions (kami veneration autochthonous to Japan, Buddhism imported from continental Asia) arrived with incommensurable cosmologies, soteriological claims, and institutional structures. Rather than compete for universal truth (as in monotheistic traditions), Japan developed parallel institutional domains: kami handle concerns of living flourishing, Buddhas handle death and salvation. This partition allowed both systems to operate without directly contradicting each other.
% FOUNDING_PROBLEM_CORROBORATION: Attested by historians of Japanese religion (Kuroda Toshio on shinbutsu shugo, Royall Tyler on religious pluralism), contemporary shrine operators and Buddhist clergy who continue to defend domain boundaries, and modern practitioners who navigate both systems. The partition is not attested as solved—it remains an operational principle in contemporary Shinto and Buddhism, though under rationalization pressure.
narrative_ontology:disappearance_verdict(shinbutsu_coexistence_commitment__domain_partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_coexistence_commitment__domain_partition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_coexistence_commitment__domain_partition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(shinbutsu_coexistence_commitment__domain_partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_coexistence_commitment__domain_partition_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_coexistence_commitment__domain_partition_reading_tests).
:- end_tests(shinbutsu_coexistence_commitment__domain_partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38) because the partition does create monopoly-like revenue streams for both institutional systems—temples control funerary rites and their associated fees, shrines control life-blessing offerings—and the constraint sustains these jurisdictional monopolies. However, extractiveness is not high because the constraint also genuinely solves a coordination problem (access to two religious resources without doctrinal conflict) and because popular participation is pragmatic rather than coerced. Suppression is moderate (0.42) because the constraint requires active institutional enforcement—theologians seeking unification must be excluded from authority, competing doctrinal frames must be suppressed, and the boundary between domains must be continuously maintained through practice and regulation. Theater is low-moderate (0.28) because the partition is substantially functional—practitioners really do use both systems, temples really do perform funerals, shrines really do bless harvests—but theater rises during periods of explicit doctrinal defense (when theologians publish honji suijaku arguments and must be refuted) and during transitions when the partition's coherence is questioned. Measurements show the constraint remained relatively stable through the pre-Meiji period, with theater rising around the Edo period (600) as Neo-Confucian rationalism pressured the system, then theater declining again by 1000 as the partition reestablished consensus. The slight extraction increase over time (0.35 to 0.40 and back to 0.38) reflects institutional elaboration and then partial destabilization.
 *
 * PERSPECTIVAL GAP:
 *   The institutional clergy and shrine operators perceive the partition as a genuine and natural coordination achievement—each system administers what it is designed to administer, and neither makes false claims about the other's domain. From their position the constraint is legitimate Rope. Popular practitioners perceive it as pragmatic and unproblematic—they use the systems as needed without internal conflict. But scholastic theologians and rationalist critics perceive the partition as incoherent evasion—they see the constraint as suppressing the genuine question of whether kami and Buddhas are ontologically unified or distinct, and from their seat the constraint operates more like Snare (suppressing alternative truth claims to protect institutional monopolies). The engine computes these divergences from the structural data: institutional beneficiaries with constrained exit compute closer to beneficiary; excluded theologians with moderate power and mobile exit compute closer to targets whose resistance is suppressed.
 *
 * DIRECTIONALITY LOGIC:
 *   Buddhist institutional clergy and kami shrine operators are beneficiaries (they collect rents from their exclusive domains, maintain institutional power, and have constrained exit—leaving the priesthood or abandoning shrine operation is costly). Popular practitioners are mild beneficiaries (they access both systems' resources without doctrinal burden, though they are powerless and trapped—exiting means losing access to either domain). The imperial court is the powerful agenda-setter (they enforce the partition through state authority and draw dual legitimacy from it; they have mobile exit because they could change policy, but constraint is advantageous so they maintain it). Scholastic theologians and modernizers are excluded and partially suppressed—their exit from the official framework is constrained by institutional power and by popular consensus that supports the partition. The partition's directionality maps to institutional power: those who benefit institutionally (clergy, court) sit at low d (beneficiaries); those who are excluded (theologians, modernizers) sit at high d (targets of suppression); popular practitioners sit near symmetric because they benefit pragmatically but bear diffuse costs if the system destabilizes.
 *
 * MANDATROPHY ANALYSIS:
 *   The domain partition reading does NOT resolve into mandatrophy (founding problem dead but constraint persists). The founding problem—managing coexistence of incommensurable religious traditions—is live and ongoing. Japanese practitioners still navigate both systems, temples still perform funerary rites, shrines still bless harvests, and the partition is still the governing principle that allows this coexistence. What HAS shifted is threat: rationalist and modernist pressure (Meiji onward) challenges the partition's coherence, but this does not constitute mandatrophy because the founding problem itself becomes contested (does Japan need this coexistence, or should it unify, separate, or rationalize away one system?). The constraint is not a zombie—it is actively defended and actively used. Mandatrophy would apply if funerary Buddhism and harvest Shinto had been replaced by secular state rituals and the constraint persisted anyway; that has not happened. The partition remains functional and is actively fought over.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_internalization_boundary,
    'Is the measured suppression (0.42) structural (institutional gatekeeping of theological authority) or partly internalized (practitioners believe they should not unify the systems)?',
    'Post-institutional-collapse suppression trajectory: if suppression persists after institutional gatekeeping is removed (e.g., after secularization or after formal shinbutsu bunri in Meiji), it indicates internalization. If suppression collapses when institutional power shifts, it is structural.',
    'If internalized, the constraint''s effective suppression is higher and more durable than the structural measure suggests—the partition would persist even if institutional enforcement were removed. If structural, suppression depends on institutional maintenance and would decay if gatekeeping authority shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_boundary, empirical, 'Structural vs. internalized suppression of unifying theology').

omega_variable(
    functional_coexistence_necessity,
    'Is the domain partition functionally necessary for Japanese religious practice (could practitioners not achieve the same practical outcomes without explicit partition rhetoric), or is it contingent institutional framing?',
    'Comparative analysis of other religious pluralisms (Hindu-Islamic coexistence, Christian-indigenous practice blending): do they require explicit domain partition or do they achieve coexistence through other framing? Evidence from Japanese practitioners'' testimony: do they describe the partition as necessary for coherence or as backdrop they do not consciously attend to?',
    'If functionally necessary, the partition is Rope (genuine coordination solution). If contingent, it may be Snare (institutional framing protecting monopolies while practitioners could coexist through other frameworks). Classification turns on whether the partition solves an authentic coordination problem or merely protects institutional rents.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(functional_coexistence_necessity, conceptual, 'Whether the domain partition is functionally necessary or institutionally contingent.').

omega_variable(
    honji_suijaku_counterfactual,
    'If honji suijaku (kami as Buddhist manifestations) had become the governing doctrine instead of domain partition, would it have been more or less extractive than the partition?',
    'Comparative institutional analysis: honji suijaku would have unified the systems under Buddhist authority, concentrating theological power and potentially creating a Buddhist monopoly on both domains. Domain partition distributes power between two institutional systems. Which arrangement extracted more from practitioners and beneficiaries?',
    'If honji suijaku would have been more extractive, the partition represents a genuine institutional compromise (Rope with modest extraction). If less extractive, the partition''s dominance was maintained through suppression of a better alternative (Snare element in the partition''s institutional enforcement).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(honji_suijaku_counterfactual, conceptual, 'Comparison of extraction under domain partition vs. honji suijaku unification').

omega_variable(
    meiji_discontinuity,
    'Was shinbutsu bunri (Meiji separation of kami and Buddha) an inevitable logical outcome of the partition''s internal contradictions, or a contingent political choice by modernizers?',
    'Historical analysis: did pre-Meiji critics of the partition argue for separation based on logical incoherence? Or was bunri imposed by external (Western rationalism, state centralization) rather than internal pressure? If internal, the partition contained seeds of its own dissolution; if external, the partition was disrupted by exogenous force.',
    'If inevitable dissolution, the partition''s coherence was always provisional (suggesting Snare with rising theater as contradictions accumulated). If contingent disruption, the partition was genuinely stable until external pressure (suggesting Rope destabilized by exogenous shock, not internal contradiction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_discontinuity, conceptual, 'Whether Meiji separation resulted from internal contradictions or external modernization pressure').

omega_variable(
    committer_alternative_framings,
    'From the standpoint of Heian-period Buddhist and Shinto intellectuals, were there coherent alternative readings of kami-Buddha coexistence besides domain partition and honji suijaku?',
    'Deep textual analysis of medieval commentarial traditions: did any intellectual stream propose a third framing (e.g., kami and Buddhas as genuinely incommensurable and requiring permanent methodological pluralism, rather than partition or fusion)? If so, why was domain partition preferred in institutional governance?',
    'If alternative readings existed and were suppressed, the partition''s dominance reflects institutional power-play (Snare element). If domain partition and honji suijaku were genuinely the only coherent options, institutional choice between them reflects different institutional interests (Rope or Tangled Rope element).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_alternative_framings, conceptual, 'Whether domain partition was the unique coherent reading or one among suppressed alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__domain_partition_reading, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(shin_tr_t200, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 200, 0.22).
narrative_ontology:measurement(shin_tr_t400, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 400, 0.24).
narrative_ontology:measurement(shin_tr_t600, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 600, 0.27).
narrative_ontology:measurement(shin_tr_t800, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 800, 0.31).
narrative_ontology:measurement(shin_tr_t1000, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 1000, 0.28).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(shin_be_t200, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 200, 0.37).
narrative_ontology:measurement(shin_be_t400, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 400, 0.38).
narrative_ontology:measurement(shin_be_t600, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 600, 0.4).
narrative_ontology:measurement(shin_be_t800, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 800, 0.42).
narrative_ontology:measurement(shin_be_t1000, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 1000, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(shin_su_t200, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 200, 0.4).
narrative_ontology:measurement(shin_su_t400, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 400, 0.42).
narrative_ontology:measurement(shin_su_t600, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 600, 0.45).
narrative_ontology:measurement(shin_su_t800, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 800, 0.48).
narrative_ontology:measurement(shin_su_t1000, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 1000, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_coexistence_commitment__domain_partition_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_coexistence_commitment__domain_partition_reading, 0.12).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__domain_partition_reading, shinbutsu_coexistence_commitment__syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__domain_partition_reading, shinbutsu_coexistence_commitment__incoherent_bundle_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__domain_partition_reading, meiji_shinbutsu_bunri).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__domain_partition_reading, japanese_religious_pluralism_institutional_structure).

% DUAL FORMULATION NOTE:
% The domain partition reading is one of three structurally distinct readings of the contested kernel 'shinbutsu_coexistence_commitment'. It frames kami and Buddhas as governing orthogonal existential domains without requiring ontological unification. The syncretic_fusion_reading frames them as unified through honji suijaku (kami as Buddhist manifestations). The incoherent_bundle_reading frames the coexistence as maintained only through ambiguity and institutional power, lacking coherent kernel. These three readings have different ε values, different beneficiary structures, different suppression mechanisms, and different classification trajectories. The domain partition reading (this file) produces a Rope-claiming constraint with moderate extraction (0.38) and moderate suppression (0.42) grounded in institutional gatekeeping. The fusion reading would produce a Tangled Rope (coordination of unification thesis + extraction of unified Buddhist authority). The incoherent-bundle reading would produce a Snare (explicit suppression of coherence questions to protect monopolies). All three are linked by network.affects_constraints; each story is generated from a single reading and does not hedge across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
