% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_commitment__partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_commitment__partition_reading, []).

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
 *   constraint_id: shinbutsu_ontological_commitment__partition_reading
 *   human_readable: Shinbutsu Domain Partition (Life-Cycle vs Afterlife) — Partition Reading
 *   domain: religious/historical/ontological
 *
 * SUMMARY:
 *   In Japan, religious practice is partitioned by domain: Shinto
 *   institutions conduct life-cycle and celebratory rites (first shrine
 *   visits, shichi-go-san, festivals, New Year worship) while Buddhist
 *   institutions conduct death rites (funerals, graves, multi-decade memorial
 *   cycles), with no ontological integration between the two — the folk
 *   formula is 'born Shinto, die Buddhist.' This file authors ONE reading of
 *   the contested kernel shinbutsu_ontological_commitment: the
 *   partition_reading, which holds that the operative commitment was stable
 *   jurisdictional separation, not metaphysical unity and not mere tolerated
 *   incoherence. The claim/metric gap is deliberate and independent: the
 *   constraint is CLAIMED as a coordination arrangement (rope) because its
 *   dominant function is a genuine division of ritual labor with plural
 *   beneficiaries and no current coercive capturer, while the authored
 *   metrics describe moderate residual extraction (0.34), low residual
 *   suppression (0.22), and rising theatricality (0.30) reflecting the
 *   danka-era enforcement arc visible in the measurement series. The engine
 *   computes per-seat classifications from the structural data; the
 *   divergence between claim and computed type is the datum, not an error to
 *   reconcile.
 *
 * KEY AGENTS:
 *   - buddhist_temple_institutions: Primary beneficiary and death-domain administrator (organized/constrained) — collects funeral, grave, and memorial revenue; administers the death side of the partition and sets prices within it
 *   - shinto_shrine_institutions: Secondary beneficiary (organized/constrained) — holds the life-cycle domain; benefits from the purity boundary while campaigning to reclaim death rites
 *   - japanese_households: Coordinated participants and residual payers (moderate/constrained) — receive a complete, socially legible ritual script; bear funeral and grave costs
 *   - family_grave_custodians: Residual targets (moderate/identity_locked) — heirs bound to grave maintenance and memorial obligations by filial identity
 *   - secular_memorial_services: Excluded alternative providers (moderate/constrained) — offer rites outside both institutions at a legitimacy discount
 *   - religious_studies_scholars: Analytical observers (analytical/analytical) — document the partition's constructed history and its contemporary erosion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_commitment__partition_reading, 0.34).
domain_priors:suppression_score(shinbutsu_ontological_commitment__partition_reading, 0.22).
domain_priors:theater_ratio(shinbutsu_ontological_commitment__partition_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, extractiveness, 0.34).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_commitment__partition_reading, rope).
narrative_ontology:human_readable(shinbutsu_ontological_commitment__partition_reading, "Shinbutsu Domain Partition (Life-Cycle vs Afterlife) — Partition Reading").
narrative_ontology:topic_domain(shinbutsu_ontological_commitment__partition_reading, "religious/historical/ontological").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_commitment__partition_reading, 'e09a12a2-1db6-4cf4-a474-07bac0c1c708').
narrative_ontology:cs_kernel_codification('e09a12a2-1db6-4cf4-a474-07bac0c1c708', distributed).
narrative_ontology:cs_authority_grounding('e09a12a2-1db6-4cf4-a474-07bac0c1c708', practice).
narrative_ontology:cs_interpretation_layer_present('e09a12a2-1db6-4cf4-a474-07bac0c1c708').
narrative_ontology:cs_reading_relation('e09a12a2-1db6-4cf4-a474-07bac0c1c708', shinbutsu_ontological_commitment__syncretic_reading, forecloses).
narrative_ontology:cs_reading_relation('e09a12a2-1db6-4cf4-a474-07bac0c1c708', shinbutsu_ontological_commitment__incoherence_reading, coexists_with).
narrative_ontology:cs_axiom('e09a12a2-1db6-4cf4-a474-07bac0c1c708', foundational, ritual_domain_separation_is_the_commitment).
narrative_ontology:cs_axiom_status(ritual_domain_separation_is_the_commitment, holdable).
narrative_ontology:cs_axiom_grounding('e09a12a2-1db6-4cf4-a474-07bac0c1c708', ritual_domain_separation_is_the_commitment, empirically_contingent).
narrative_ontology:cs_axiom('e09a12a2-1db6-4cf4-a474-07bac0c1c708', foundational, practitioner_autonomy_over_doctrine).
narrative_ontology:cs_axiom_status(practitioner_autonomy_over_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('e09a12a2-1db6-4cf4-a474-07bac0c1c708', practitioner_autonomy_over_doctrine, instrumental).
narrative_ontology:cs_reference_frame('e09a12a2-1db6-4cf4-a474-07bac0c1c708', stable_dual_domain_partition).
narrative_ontology:cs_drift_state('e09a12a2-1db6-4cf4-a474-07bac0c1c708', contemporary_post_secularization, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e09a12a2-1db6-4cf4-a474-07bac0c1c708', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_commitment__partition_reading, shinbutsu_ontological_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__partition_reading, buddhist_temple_institutions).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__partition_reading, shinto_shrine_institutions).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__partition_reading, japanese_households).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__partition_reading, family_grave_custodians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__partition_reading, japanese_households).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_commitment__partition_reading, shinto_death_pollution_taboo).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_commitment__partition_reading, ie_lineage_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the death-rite domain: funerals, graves, and memorial services across multi-decade commemorative cycles. Collect funeral fees, grave purchase and maintenance payments, and periodic memorial offerings from registered parish households. Administer the death side of the partition and set prices within it; a temple cannot abandon death rites without surrendering its economic base, and rural temples face succession collapse if that revenue thins further.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, buddhist_temple_institutions, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__partition_reading, buddhist_temple_institutions, agenda_setter).

% Hold the life-cycle domain: first shrine visits, children's blessing milestones, festivals, New Year worship, and less consistently weddings. Receive offerings, talisman and amulet sales, and festival participation. Maintain the death-purity boundary that keeps funerals off shrine grounds; since the Meiji separation they are institutionally distinct from temples, and some now campaign to develop Shinto funeral rites to reclaim the death domain.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, shinto_shrine_institutions, beneficiary,
    organized, generational, constrained, national).

% Follow the inherited script: shrine visits for births and seasonal observances, temple funerals and ancestral graves for deaths. Gain a complete, socially legible ritual calendar without negotiating between providers at each life event; pay funeral costs, grave purchase, and annual maintenance along the way. Exit is possible — secular funerals and grave-free memorials exist — but carries family friction and the sense of abandoning the ancestors.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, japanese_households, beneficiary,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__partition_reading, japanese_households, payer).

% Usually eldest sons or designated heirs who inherit custody of the family grave and the obligation to fund its maintenance and request memorial services, whether or not they hold Buddhist belief. The obligation is fused with filial identity: letting the grave lapse reads as severing the lineage and dishonoring parents. Some exercise exit through grave-free joint memorials or scattering, at the cost of family conflict.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, family_grave_custodians, payer,
    moderate, biographical, identity_locked, national).

% Commercial and civic providers — funeral halls, scattering services, family-funeral operators — offering rites outside both institutions. Legally unobstructed since 1945 but legitimacy-discounted: families fear social judgment for skipping temple rites, and the providers sit outside the customary conversation about how death should be handled.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, secular_memorial_services, excluded,
    moderate, biographical, constrained, national).

% Analyze the historical formation of kami-buddha relations — medieval system scholarship, Meiji separation-edict studies, survey research on ritually observant but non-religious Japanese. They document that the partition is a contingent, historically enforced arrangement rather than an eternal feature, and track its erosion in contemporary practice.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, religious_studies_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_commitment__partition_reading, buddhist_temple_institutions).
narrative_ontology:fixing_cost_class(shinbutsu_ontological_commitment__partition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Divides ritual jurisdiction between two institutions so every household has a complete, conflict-free script across the life course: shrines handle purity, celebration, and life transitions; temples handle pollution, death, and ancestral continuity — solved once, institutionally, instead of per-household negotiation between competing ritual providers.
% TRANSFER_FUNCTION: Moves death-rite fees, grave purchase and maintenance payments, and memorial offerings from households to Buddhist temples; moves offering, talisman, and attendance flows toward shrines; historically also moved compulsory registration compliance from households to the Tokugawa state's surveillance apparatus.
% ABSENT_VOICES: Secular and non-Buddhist memorial providers, households who reject ancestral obligations, and (before 1868) Christian communities — kept out of the death-rite conversation by custom and, earlier, by law. Also the pre-Buddhist kami cults whose death-pollution taboo is the reason the death domain was ceded at all: the original excluded voice shaped the partition by absence.
% DISAPPEARANCE_RATIONALE: If the partition vanished overnight, households would face unscripted choices at every rite of passage; temples would lose the death-rite economy that sustains them; shrines would face sudden funeral demand or a jurisdictional vacuum; the funeral industry would reorganize around contested providers; and the ancestral-rite calendar binding generations of custodians to graves would dissolve. Arrangements demonstrably depend on it.
% FOUNDING_PROBLEM: Buddhism's arrival in sixth-century Japan created a jurisdictional collision: kami worship treated death as pollution and could not handle corpses or mourning, while Buddhism offered afterlife care and ancestral memorialization. The arrangement was built to allocate death care and life celebration between two rival ritual systems without open institutional war.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: the Nihon shoki and clan chronicles attest the original collision (Nakatomi and Mononobe resistance to Buddhist death rites); Edo-period terauke registration records show the death-rite monopoly was state-compelled rather than household-demanded; Kuroda Toshio's medieval scholarship and Meiji separation-edict histories attest the partition's constructed, administered character; NHK and university survey series showing a non-religious majority performing both shrine and temple rites attest that persistence runs on custom rather than resolved conviction. No beneficiary attestation is relied upon.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_commitment__partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_commitment__partition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_commitment__partition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(shinbutsu_ontological_commitment__partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_commitment__partition_reading, 0.34, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_commitment__partition_reading_tests).
:- end_tests(shinbutsu_ontological_commitment__partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.34 reflects a concentrated but decaying death-rite fee stream: the series shows the danka-era peak (0.60 at 1800, when registration was compulsory and the funeral monopoly state-backed) collapsing after the 1868 separation edicts and 1871 danka abolition, then flattening as custom rather than law sustains revenue. Suppression 0.22 is the residual customary pressure (family expectation, cemetery economics) after legal compulsion was dismantled — note suppression is authored as a raw structural property and is NOT scaled by power or scope; only extractiveness is scaled by the engine. Theater_ratio 0.30 and rising tracks the soshiki-bukkyo critique: rites performed competently but without doctrinal understanding, participation becoming rote. Accessibility_collapse 0.42: alternatives (secular funerals, Christian weddings, grave-free memorials) remain visible and increasingly chosen, so alternatives are only partly collapsed. Resistance 0.30: funeral-cost complaints, secular-memorial advocacy, and — notably — the shrine establishment's own campaign to develop Shinto funeral rites, which is resistance to the partition's death-domain assignment mounted from inside the beneficiary set. All three metric series share one time grid (1615-2026, eight points) so the engine samples aligned rows; the trajectories are monotonic arcs (enforcement decay, extraction decay, theater rise), not cycles.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the temple seat the arrangement is pastoral vocation and economic base — extraction is invisible from inside the collecting institution. From the grave-custodian seat the same structure is an obligatory cost fused with filial identity — the highest-directionality experience in the story. From the household seat it is a convenience: a complete ritual calendar requiring no per-event negotiation. From the scholar seat it is a contingent, historically enforced formation now eroding. The engine derives these divergences from power, exit, and beneficiary/victim declarations; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Buddhist temples and Shinto shrines sit near the beneficiary end: the partition subsidizes both with protected jurisdiction (shrines additionally collect from the purity boundary that keeps death off their grounds). Japanese households sit near symmetric — genuine coordination benefit (scripts, legibility) against diffuse payment exposure, captured by their dual beneficiary/payer declaration. Family grave custodians sit nearest the target end: they bear the concentrated residual cost, and their identity_locked exit amplifies effective extraction — the obligation is fused with lineage-and-filial identity, so exit is experienced as self-betrayal rather than mere inconvenience. The national spatial scope modestly amplifies effective extraction on targets per the engine's scope modifier. No directionality overrides are authored: the beneficiary/victim declarations plus exit atoms already produce the correct structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — allocating death care between two rival ritual systems without open institutional war — was substantively resolved centuries ago: Shinto's death-pollution taboo conceded the corpse to Buddhism, and the rivalry the partition was built to manage no longer exists. Yet the arrangement persists because it still delivers live goods: complete ritual scripts, ancestral continuity, and protected revenue bases. The mandatrophy guard cuts both ways. Reading the partition as pure extraction (snare) ignores the genuine coordination function and the absence of any current coercive capturer; reading it as natural or inevitable (mountain) ignores its constructed, state-enforced history. The honest current state is a coordination arrangement with early mandatrophy drift: theater_ratio has risen monotonically (0.10 to 0.30) and the drift_state records substantial acknowledged practice erosion. If secularization completes the drift — abandoned graves, successor-less temples, majority non-religious identification — expect transition toward an inertia-maintained remainder unless the shrines' funeral campaign or temple adaptation re-functionalizes the death domain. The measurement series exists precisely to date that transition if it comes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This file instantiates the partition_reading of kernel shinbutsu_ontological_commitment; what would the sibling readings change structurally, and where exactly is the disagreement located?',
    'Adopting the syncretic reading replaces this constraint with one in which kami and buddhas form a single honji-suijaku order — temples become metaphysical superiors, shrines subordinate vessels, and the beneficiary structure consolidates under the Buddhist establishment; adopting the incoherence reading dissolves the constraint entirely (no stable commitment, hence no epsilon-bearing arrangement to classify). The disagreement is located at a single point: whether enacted domain-separation itself constituted the operative ontological commitment.',
    'Classification is reading-indexed: the same historical material classifies as a low-extraction coordination arrangement under this reading, as a hierarchical unity (likely carrying asymmetric extraction favoring Buddhist institutions) under the syncretic reading, and as nothing classifiable under the incoherence reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame indexicality: one reading of the shinbutsu kernel; siblings would restructure beneficiaries or dissolve the constraint.').

omega_variable(
    incoherence_relation_level_ambiguity,
    'Does the partition reading foreclose or merely coexist with the incoherence reading? The declared edge (coexists_with) rests on distinguishing articulated ontology (unintegrated — where the incoherence reading operates) from operative commitment (stable domain-separation — where this reading operates); a stricter framing that levels both onto one plane makes the readings directly contradictory.',
    'Conceptual analysis of what counts as an ontological commitment: if commitments must be articulated to exist, the partition reading collapses into the incoherence reading and the edge should be forecloses; if enacted practice can constitute commitment, coexists_with stands.',
    'Under the leveled framing this reading loses its distinctness — the corpus would carry one fewer reading and the kernel''s classification space narrows to syncretic-versus-incoherent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incoherence_relation_level_ambiguity, conceptual, 'Level ambiguity governing the partition-to-incoherence relation edge.').

omega_variable(
    danka_extraction_residue,
    'How much of the measured extractiveness is living residue of the compulsory Edo-period danka registration system versus a fair price for funeral, grave, and memorial services?',
    'Compare temple funeral and grave-maintenance pricing against secular-provider equivalents and disclosed temple cost structures; historical comparison of death-rite pricing before and after the 1871 danka abolition.',
    'If most of the 0.34 is danka residue, the arrangement computes closer to a hybrid with temples as concentrated capturers of a decaying compulsion; if it tracks service cost, the pure-coordination claim strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(danka_extraction_residue, empirical, 'Whether residual death-rite extraction is rent or price.').

omega_variable(
    grave_custodian_exit_softening,
    'Is the identity-fused exit position of family grave custodians softening as abandoned graves (aki haka), grave-free permanent memorials, and scattering services normalize?',
    'Track cemetery-operator data on grave abandonment, uptake of tree-burial and joint graves, and cohort survey series on ancestral-obligation attitudes.',
    'If exit normalizes, custodians shift toward constrained or mobile positions, effective extraction on the payer seat falls, and the partition''s residual extraction decays faster than the base rate suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(grave_custodian_exit_softening, empirical, 'Trajectory of the payer seat''s identity lock.').

omega_variable(
    meiji_artifact_hypothesis,
    'Was the clean two-domain partition substantially manufactured by the Meiji shinbutsu bunri edicts — imposed separation where lived practice was syncretic — such that the partition as described is an administrative artifact rather than an organic practitioner settlement?',
    'Pre-Meiji parish and household records showing combined shrine-temple practice; regional variation studies where separation enforcement was weaker.',
    'If artifact, the partition''s persistence leans on institutional inertia rather than practitioner need, weakening the coordination-first reading and raising decay-or-performance risk; if organic, the coordination-first reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_artifact_hypothesis, empirical, 'Whether the partition is a Meiji administrative product or an older practitioner settlement.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the residual suppression around death-rite conformity structural (customary sanction, family expectation, cemetery economics) or internalized (filial duty experienced as one''s own desire)?',
    'Post-exit trajectory: custodians who adopt grave-free memorials — does the felt obligation dissipate (structural) or persist as guilt (internalized)? Cohort attitude surveys across generations.',
    'If internalized, effective suppression exceeds the 0.22 structural measure — the constraint travels with the agent after exit; if structural, normalization of alternatives will dissolve it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized mechanism of residual death-rite suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_commitment__partition_reading, 1615, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t1615, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 1615, 0.1).
narrative_ontology:measurement(shin_tr_t1700, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 1700, 0.11).
narrative_ontology:measurement(shin_tr_t1800, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 1800, 0.13).
narrative_ontology:measurement(shin_tr_t1868, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 1868, 0.16).
narrative_ontology:measurement(shin_tr_t1920, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 1920, 0.17).
narrative_ontology:measurement(shin_tr_t1945, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 1945, 0.2).
narrative_ontology:measurement(shin_tr_t1980, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 1980, 0.26).
narrative_ontology:measurement(shin_tr_t2026, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 2026, 0.3).

% Extraction over time
narrative_ontology:measurement(shin_be_t1615, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 1615, 0.55).
narrative_ontology:measurement(shin_be_t1700, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 1700, 0.58).
narrative_ontology:measurement(shin_be_t1800, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 1800, 0.6).
narrative_ontology:measurement(shin_be_t1868, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 1868, 0.48).
narrative_ontology:measurement(shin_be_t1920, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 1920, 0.42).
narrative_ontology:measurement(shin_be_t1945, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 1945, 0.37).
narrative_ontology:measurement(shin_be_t1980, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 1980, 0.35).
narrative_ontology:measurement(shin_be_t2026, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 2026, 0.34).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t1615, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 1615, 0.7).
narrative_ontology:measurement(shin_su_t1700, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 1700, 0.73).
narrative_ontology:measurement(shin_su_t1800, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 1800, 0.74).
narrative_ontology:measurement(shin_su_t1868, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 1868, 0.5).
narrative_ontology:measurement(shin_su_t1920, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 1920, 0.32).
narrative_ontology:measurement(shin_su_t1945, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 1945, 0.26).
narrative_ontology:measurement(shin_su_t1980, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 1980, 0.24).
narrative_ontology:measurement(shin_su_t2026, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 2026, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_commitment__partition_reading, resource_allocation).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__partition_reading, shinbutsu_ontological_commitment__syncretic_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__partition_reading, shinbutsu_ontological_commitment__incoherence_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'shinbutsu-shugo' conflates three structurally distinct claims about the ontological commitment — metaphysical unity under honji-suijaku (syncretic_reading), stable jurisdictional partition without integration (this file), and institutionally tolerated incoherence (incoherence_reading). Each claim gets its own epsilon, beneficiary structure, and classification; they are linked here via affects_constraints rather than forced into one story with a measurement parameter. Genealogical ordering: the syncretic reading is the medieval doctrinal frame the Meiji state dismantled; this partition reading is the post-separation settlement; the incoherence reading is the modern scholarly characterization of what remains. The upstream syncretic story influences this one historically (its demolition by the 1868 edicts is what produced the clean partition this reading describes).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
