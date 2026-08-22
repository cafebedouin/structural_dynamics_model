% ============================================================================
% CONSTRAINT STORY: simultaneous_veneration__pragmatic_incoherence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_simultaneous_veneration__pragmatic_incoherence_reading, []).

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
 *   constraint_id: simultaneous_veneration__pragmatic_incoherence_reading
 *   human_readable: Simultaneous Kami-Buddha Veneration — Pragmatic Incoherence Reading
 *   domain: religious_studies/japanese_history
 *
 * SUMMARY:
 *   Pre-Meiji Japan maintained a single institutional interface — the
 *   shrine-temple complex (jingūji and its kin) — through which communities
 *   addressed both the kami of this-worldly efficacy and the buddhas of
 *   salvation. The colloquial label 'shinbutsu shūgō' names this arrangement,
 *   but the label covers three structurally distinct claims, authored as
 *   separate stories per the epsilon-invariance principle: that the two cults
 *   were ontologically one (ontological_fusion_reading), that they were
 *   functionally partitioned (domain_partition_reading), and — this file —
 *   that the arrangement was never coherent at all: practitioners held
 *   contradictory beliefs side by side without resolution, and the
 *   arrangement persisted not because anything held it together but because
 *   no authority ever applied the pressure that would have exposed the void.
 *   On this reading the standing arrangement under assessment is the
 *   pre-Meiji fused complex as actually practiced, and epsilon is indexed to
 *   this reading's view of it: high, because the arrangement charged its
 *   constituents unresolved contradiction and doubled institutional claims
 *   under a doctrinal cover (honji-suijaku) that never delivered the
 *   reconciliation it advertised. The Meiji separation edicts (1868–1871) are
 *   modeled as the natural experiment this reading predicts: pressure
 *   arrived, and the arrangement dissolved almost without defense —
 *   revelation of latent incoherence, not imposed rupture. Claim and metrics
 *   are authored independently: claimed_type states the structure this
 *   reading sees; the metrics describe the operation the record shows. KEY
 *   AGENTS (by structural relationship): - temple_shrine_establishments:
 *   Administering beneficiary (institutional/constrained) — runs the combined
 *   rite and collects both registers of support - lay_devotees: Primary cost
 *   bearer (powerless/constrained) — carries the unresolved dual obligation -
 *   honji_suijaku_scholastics: Doctrinal maintenance crew
 *   (moderate/identity_locked) — absorbs the unresolvable labor -
 *   kokugaku_scholars: Excluded objectors (moderate/constrained) — a century
 *   of recorded objection, no seat - bakufu_religious_magistracies:
 *   Non-adjudicating administrator (institutional/arbitrage) — could have
 *   forced resolution, chose not to - meiji_reform_oligarchs: Enforcing
 *   successor (institutional/arbitrage) — applied the pressure that tested
 *   the arrangement - historians_of_japanese_religion: Analytical observer —
 *   sees the full structure across doctrine, administration, and the
 *   separation test
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simultaneous_veneration__pragmatic_incoherence_reading, 0.78).
domain_priors:suppression_score(simultaneous_veneration__pragmatic_incoherence_reading, 0.08).
domain_priors:theater_ratio(simultaneous_veneration__pragmatic_incoherence_reading, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 0.72).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, resistance, 0.32).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simultaneous_veneration__pragmatic_incoherence_reading, piton).
narrative_ontology:human_readable(simultaneous_veneration__pragmatic_incoherence_reading, "Simultaneous Kami-Buddha Veneration — Pragmatic Incoherence Reading").
narrative_ontology:topic_domain(simultaneous_veneration__pragmatic_incoherence_reading, "religious_studies/japanese_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simultaneous_veneration__pragmatic_incoherence_reading, '258e4b5d-876f-4573-b4ad-bd93c74e5552').
narrative_ontology:cs_kernel_codification('258e4b5d-876f-4573-b4ad-bd93c74e5552', distributed).
narrative_ontology:cs_authority_grounding('258e4b5d-876f-4573-b4ad-bd93c74e5552', practice).
narrative_ontology:cs_interpretation_layer_present('258e4b5d-876f-4573-b4ad-bd93c74e5552').
narrative_ontology:cs_reading_relation('258e4b5d-876f-4573-b4ad-bd93c74e5552', simultaneous_veneration__ontological_fusion_reading, influences).
narrative_ontology:cs_reading_relation('258e4b5d-876f-4573-b4ad-bd93c74e5552', simultaneous_veneration__domain_partition_reading, forecloses).
narrative_ontology:cs_axiom('258e4b5d-876f-4573-b4ad-bd93c74e5552', foundational, unexamined_contradiction_is_practices_default_state).
narrative_ontology:cs_axiom_status(unexamined_contradiction_is_practices_default_state, holdable).
narrative_ontology:cs_axiom_grounding('258e4b5d-876f-4573-b4ad-bd93c74e5552', unexamined_contradiction_is_practices_default_state, empirically_contingent).
narrative_ontology:cs_axiom('258e4b5d-876f-4573-b4ad-bd93c74e5552', foundational, unenforced_persistence_is_no_evidence_of_coherence).
narrative_ontology:cs_axiom_status(unenforced_persistence_is_no_evidence_of_coherence, holdable).
narrative_ontology:cs_axiom_grounding('258e4b5d-876f-4573-b4ad-bd93c74e5552', unenforced_persistence_is_no_evidence_of_coherence, empirically_contingent).
narrative_ontology:cs_reference_frame('258e4b5d-876f-4573-b4ad-bd93c74e5552', customary_unadjudicated_parallel_practice).
narrative_ontology:cs_drift_state('258e4b5d-876f-4573-b4ad-bd93c74e5552', meiji_separation_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('258e4b5d-876f-4573-b4ad-bd93c74e5552', '').
narrative_ontology:cs_kernel_id(simultaneous_veneration__pragmatic_incoherence_reading, simultaneous_veneration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simultaneous_veneration__pragmatic_incoherence_reading, temple_shrine_establishments).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__pragmatic_incoherence_reading, lay_devotees).
narrative_ontology:constraint_victim(simultaneous_veneration__pragmatic_incoherence_reading, lay_devotees).
narrative_ontology:constraint_victim(simultaneous_veneration__pragmatic_incoherence_reading, honji_suijaku_scholastics).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__pragmatic_incoherence_reading, honji_suijaku_scholastics).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operated the combined complexes: the same corporate body held the shrine's lands, housed the buddha images beside the kami, sent its monks to perform kami rites, and kept two registries of dues and festival obligations over the same parish households. Charters, tax exemptions, and festival economies all presumed the combination held; unwinding it meant re-litigating land title, retraining clergy, and rebuilding calendars, so even complexes sympathetic to purifying the kami cult rarely moved first.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, temple_shrine_establishments, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__pragmatic_incoherence_reading, temple_shrine_establishments, beneficiary).

% Attended the festivals, paid the dues, and called on the same local complex for rain, healing, childbirth protection, and funerals, addressing its kami and its buddhas as occasion required. Obligations ran to one institution wearing two faces; opting out of either register meant standing apart from village ritual life, since the calendar, the guilds, and the burial grounds all routed through the complex.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, lay_devotees, payer,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__pragmatic_incoherence_reading, lay_devotees, beneficiary).

% Monk-doctors of the great complexes who produced the reconciliation literature: tracing the kami to Buddhist originals, ranking the manifestations, harmonizing the miracle tales. The work could never close — every new local deity and every rival lineage's genealogy reopened it — yet their chairs, curricula, and polemical reputations were staked on the project continuing. Leaving it meant abandoning the framework that made their scholarship intelligible.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, honji_suijaku_scholastics, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__pragmatic_incoherence_reading, honji_suijaku_scholastics, beneficiary).

% Nativist philologists who argued the kami were underived natives of the land and the Buddhist overlay a corruption to be scraped off. They published, taught, and built networks outside the establishment complexes, but held no seat in any body that governed temple-shrine affairs; their objection stood on record for a century before anyone with authority acted on it.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, kokugaku_scholars, excluded,
    moderate, generational, constrained, national).

% The warrior government's temple-shrine offices registered every priest, licensed every complex, and policed sectarian conflict down to forced debates, yet left the kami-buddha combination entirely unadjudicated — treating it as custom too old and too useful to touch. They could have ordered separation at any point across two centuries of active religious administration; the decision not to decide was theirs, and each generation of magistrates renewed it.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, bakufu_religious_magistracies, agenda_setter,
    institutional, generational, arbitrage, national).

% The restoration government that inherited the arrangement and, advised by nativist scholars, ordered kami and buddhas separated by edict in 1868. They found no organized defense mounted: complexes petitioned for adjustments, a few resisted locally, and most simply reclassified their images, clergy, and lands within a few years. They then discovered that the purified shrine cult they believed they were restoring had to be invented nearly from scratch.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, meiji_reform_oligarchs, agenda_setter,
    institutional, generational, arbitrage, national).

% Modern scholarship reconstructs the arrangement from registry books, ritual manuals, and village records. It holds the analytical seat from which the whole structure — doctrine, administration, practice, and the separation test — is visible at once, and from which competing accounts of the same material are compared.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, historians_of_japanese_religion, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(simultaneous_veneration__pragmatic_incoherence_reading, temple_shrine_establishments).
narrative_ontology:fixing_cost_class(simultaneous_veneration__pragmatic_incoherence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gave each community one institutional interface covering both registers of religious power — this-worldly efficacy (rain, healing, protection) and posthumous welfare — with a single festival calendar, a single building complex, a single clergy, and a single set of dues replacing what would otherwise be two parallel institutions competing for the same households.
% TRANSFER_FUNCTION: Moved dues, offerings, festival labor, and pilgrimage spending from lay households to the combined complexes under both registers at once; moved doctrinal labor from the scholastic class into public ritual performance; and placed the cost of unresolved contradiction on the practitioners and specialists least positioned to refuse it.
% ABSENT_VOICES: The kokugaku scholars and their Neo-Confucian predecessors objected in print for a century and held no seat in any body governing temple-shrine affairs; skeptical villagers left no record because literacy and forum were both withheld from them; and on the nativist account the kami themselves were the aggrieved party, their cult subordinated without consultation.
% DISAPPEARANCE_RATIONALE: When pressure finally arrived, the world rearranged within years: complexes split their registers, reassigned lands and clergy, destroyed or reclassified images, rewrote festival calendars, and the purified shrine cult had to be invented nearly from scratch — demonstrating that a dense web of land tenure, ritual schedule, and clerical career depended on the arrangement, and that nothing beneath it was prepared to hold.
% FOUNDING_PROBLEM: Medieval Japan needed to house an imported, literate, textually sophisticated salvation religion alongside an indigenous, localized cult of this-worldly efficacy without either absorbing or annihilating the other; the fused complex solved that housing problem institutionally.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: Edo-period Neo-Confucian critics and kokugaku philologists (Motoori Norinaga's rejection of honji-suijaku as Buddhist contamination; Hirata-school purification demands) attested on record, a century before separation, that the synthesis answered no live doctrinal question and survived as administrative custom; the Meiji reformers acted on precisely that external testimony. Establishment voices defended the arrangement's antiquity and revenues, never its continuing function — no insider attests the founding problem was still live, and that absence of self-interested corroboration for liveness is itself the signal.
narrative_ontology:disappearance_verdict(simultaneous_veneration__pragmatic_incoherence_reading, world_rearranges).
narrative_ontology:founding_problem_status(simultaneous_veneration__pragmatic_incoherence_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(simultaneous_veneration__pragmatic_incoherence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(simultaneous_veneration__pragmatic_incoherence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(simultaneous_veneration__pragmatic_incoherence_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(simultaneous_veneration__pragmatic_incoherence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(simultaneous_veneration__pragmatic_incoherence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(simultaneous_veneration__pragmatic_incoherence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Time units are decades: t0 is approximately the 1180s (the medieval synthesis at full articulation), t69 the 1870s (separation executed and consolidated). Extractiveness rises monotonically (0.34 to 0.78) as the synthesis ossifies: early honji-suijaku was live intellectual work whose costs were offset by generative payoff; by the Edo period the doctrine was formula, the gap between official theory and actual practice had widened for generations, and the accumulated suppressed contradiction — plus the doubled claims both registers kept on the same households — surfaces in the terminal rise as separation forces realization of costs long carried unacknowledged. Suppression_requirement FALLS across the whole interval (0.27 to 0.08): this is the story's spine, and the series is authored precisely because enforcement-capacity change is the dynamic under trace — the arrangement required ever less active defense as it passed from doctrine into habit, and at the end required none, collapsing on first contact with pressure. Theater_ratio rises (0.14 to 0.72) as maintenance shifts from argument to ceremony: annual rites and formal genealogies continued to affirm the fusion while living doctrinal energy migrated to sect-specific projects. All three series share one grid (t = 0, 10, 20, 30, 40, 50, 60, 65, 69) so no metric is sampled against another's end-state. Accessibility_collapse is low (0.30) because alternatives never needed suppressing: exclusive nembutsu practice, Yoshida's inverted hierarchy, and nativist critique all remained visible and practicable inside the arrangement's shadow. Resistance is moderate (0.32): a literate opposition tradition existed for a century before separation but never mobilized mass defection, because the arrangement demanded little enough that bearing it stayed cheaper than fighting it; a devotee coalition never formed for the same reason — costs were diffuse, unfelt, and forumless. The dynamics are monotonic drift with terminal collapse, not cyclical, so no oscillation mechanism is claimed.
 *
 * PERSPECTIVAL GAP:
 *   Four constituent seats, four different arrangements. From the complex's seat the fusion is ancestral custom constituting its entire economy. From the devotee's seat it is an unchosen double obligation woven into village life. From the scholastic's seat it is an assignment that could never be closed, borne under professional identity lock: chairs, curricula, and polemical standing were constituted BY the reconciliation project, so exit was unthinkable until the framework itself was abolished — at which point the obligation vanished with the job, which is exactly what the separation record shows. Break the identity frame and that seat's cost-bearing collapses to zero overnight. From the magistrate's seat the whole question was a convenience not worth adjudicating. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to d as follows. The complexes sit near the beneficiary end — they collect both registers and administer the rite (structural derivation; no override). Lay devotees carry a powerless-atom override to d = 0.55: their dual listing as beneficiary and victim would derive a symmetric 0.5, but this reading's specific claim — that the contradiction's costs surface onto the practitioners who carry them — tips them slightly targetward while preserving their real service benefit. The scholastics derive high d from victim status, amplified by identity_locked exit. The kokugaku scholars are excluded rather than coordinated: outside the flow, their objection fed no register. The magistrates sit near the beneficiary end via option value — non-adjudication was cheap governance, renewed each generation. The Meiji oligarchs enter the structure only as the enforcers whose pressure tests it; they are not constituents. Suppression is authored as a raw structural property, unscaled by power or scope, and is genuinely low here: the arrangement's persistence without enforcement is the finding, not an artifact of computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification work this story performs is triage among mislabels. Calling the arrangement pure coordination launders the suppressed-contradiction costs as overhead and credits a coherence the record cannot locate. Calling it pure extraction-by-coercion requires an enforcement machinery that never existed — persistence WITHOUT enforcement is the opposite signature. The structure the record supports is a former live synthesis atrophied into inertial co-performance: the founding problem was solved or dissolved centuries before separation (status: dead), yet arrangements demonstrably depended on the complex (world rearranges on removal) — the dead-problem/live-dependence mismatch that flags the zombie form. The Meiji collapse-on-contact is the differential test: coercion-backed structures resist their abolition; inertial ones evaporate. The complexes' swift surrender — petitioning for adjustment, reclassifying images, rebranding clergy within years — shows their benefit rode on customary catchment rather than on any fusion worth defending, which is why no concentrated defender appears despite concentrated receipt. Receipt and defense come apart, and the gap is the diagnosis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This file instantiates only the pragmatic_incoherence_reading of the kernel simultaneous_veneration; epsilon here is indexed to that reading''s assessment of the standing pre-Meiji arrangement. What would the sibling readings'' epsilons be over the same referent, and does the kernel admit any reading-free assessment?',
    'Not resolvable by data alone: generate the sibling files (ontological_fusion_reading, domain_partition_reading) over the identical referent and compare per-seat classifications. The disagreement is located in the epistemic status of practitioner belief: contradictory-and-unresolved (this reading), metaphysically unified (fusion), or domain-governed (partition).',
    'Under the fusion reading the suppressed-contradiction charge vanishes and epsilon collapses toward the coordination-cost floor; under the partition reading epsilon sits low-to-moderate with the arrangement reading as functional specialization. This file''s classification is valid only under this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed epsilon over a contested kernel; sibling readings are separate constraints, not measurement parameters of this one.').

omega_variable(
    latent_vs_experienced_contradiction,
    'Was the contradiction practitioners carried latent (compartmentalized at no subjective cost, visible only to doctrine) or experienced (felt tension with no resolution forum available)?',
    'Post-separation reception records: village petitions, diaries, clerical memoirs, and the speed and local texture of compliance — relief, indifference, or grief at separation discriminates the two conditions.',
    'If compartmentalization was costless, the arrangement drifts toward the partition reading''s territory and epsilon drops sharply; if tension was felt but forumless, the high-extraction assessment stands and ''suppressed'' is literal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(latent_vs_experienced_contradiction, empirical, 'Whether the suppressed contradiction was subjectively borne or merely structurally present.').

omega_variable(
    counterfactual_enforcement_test,
    'Would earlier enforcement pressure have dissolved the arrangement centuries before Meiji, as the ''sustained by lack of enforcement'' claim implies?',
    'Comparative micro-cases where pressure was applied locally — Yoshida Shinto''s attempted hierarchy inversion, imposed sectarian debates, early-Meiji prefectural zeal exceeding the edicts — measuring dissolution speed and the organization of any defense.',
    'Fast, undefended dissolution across cases confirms inertia-only persistence; organized, costly defense anywhere would indicate a hidden coordination function and force revision toward a hybrid coordination/extraction account.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_enforcement_test, empirical, 'Natural-experiment test of the enforcement-absence persistence mechanism.').

omega_variable(
    non_adjudication_capacity_or_choice,
    'Was the warrior government''s failure to adjudicate the kami-buddha question an incapacity (no legitimate arbiter existed) or a choice (non-adjudication was cheap governance, renewed by each generation of magistrates)?',
    'Magisterial records: did the temple-shrine offices decline jurisdiction with stated reasons; did they arbitrate property and rank disputes vigorously while waving off doctrinal ones?',
    'If incapacity, the arrangement persisted in a vacuum and the inertial reading strengthens; if deliberate tolerance, the state was a silent sponsor collecting social peace, and persistence was subsidized rather than merely inertial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_adjudication_capacity_or_choice, empirical, 'Whether enforcement absence reflects state incapacity or state strategy.').

omega_variable(
    revelation_or_rupture,
    'Did the Meiji separation succeed because the arrangement was hollow (revelation of latent incoherence, this reading''s claim) or because restoration-state coercion was overwhelming (imposed rupture)?',
    'Compliance speed and distribution measured against coercive capacity actually deployed; presence or absence of organized defense; the nativists'' own post-separation disappointment as the purified cult they expected failed to revive.',
    'Revelation evidence validates the reading''s distinctive claim and the terminal extractiveness spike; rupture evidence transfers explanatory weight to state power and partially rehabilitates the arrangement''s grip on practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revelation_or_rupture, empirical, 'Distinguishes hollow-arrangement collapse from coercion-imposed collapse at the separation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simultaneous_veneration__pragmatic_incoherence_reading, 0, 69).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(simu_tr_t0, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(simu_tr_t10, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 10, 0.21).
narrative_ontology:measurement(simu_tr_t20, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 20, 0.29).
narrative_ontology:measurement(simu_tr_t30, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 30, 0.37).
narrative_ontology:measurement(simu_tr_t40, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 40, 0.45).
narrative_ontology:measurement(simu_tr_t50, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 50, 0.53).
narrative_ontology:measurement(simu_tr_t60, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 60, 0.61).
narrative_ontology:measurement(simu_tr_t65, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 65, 0.67).
narrative_ontology:measurement(simu_tr_t69, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 69, 0.72).

% Extraction over time
narrative_ontology:measurement(simu_be_t0, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(simu_be_t10, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(simu_be_t20, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(simu_be_t30, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 30, 0.5).
narrative_ontology:measurement(simu_be_t40, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(simu_be_t50, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 50, 0.6).
narrative_ontology:measurement(simu_be_t60, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 60, 0.66).
narrative_ontology:measurement(simu_be_t65, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 65, 0.71).
narrative_ontology:measurement(simu_be_t69, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 69, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(simu_su_t0, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 0, 0.27).
narrative_ontology:measurement(simu_su_t10, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 10, 0.25).
narrative_ontology:measurement(simu_su_t20, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 20, 0.23).
narrative_ontology:measurement(simu_su_t30, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 30, 0.21).
narrative_ontology:measurement(simu_su_t40, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 40, 0.19).
narrative_ontology:measurement(simu_su_t50, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 50, 0.17).
narrative_ontology:measurement(simu_su_t60, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 60, 0.14).
narrative_ontology:measurement(simu_su_t65, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 65, 0.11).
narrative_ontology:measurement(simu_su_t69, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 69, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simultaneous_veneration__pragmatic_incoherence_reading, identity_coordination).
narrative_ontology:affects_constraint(simultaneous_veneration__pragmatic_incoherence_reading, ontological_fusion_reading).
narrative_ontology:affects_constraint(simultaneous_veneration__pragmatic_incoherence_reading, domain_partition_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'shinbutsu shugō' (simultaneous kami-buddha veneration) decomposes, per the epsilon-invariance principle, into three structurally distinct claims with different epsilon over the same referent. This file is the pragmatic_incoherence_reading (never coherent; contradiction unresolved; sustained by enforcement absence). ontological_fusion_reading holds the two cults ontologically one (honji-suijaku as metaphysical truth) and authors low epsilon; domain_partition_reading holds them functionally partitioned and authors low-to-moderate epsilon. Upstream/downstream: the fusion reading historically supplied the doctrine the arrangement cited for itself; the partition reading is the modern scholarly default; this reading is the minority position whose decisive test case is the Meiji transition. Each member links the others here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(simultaneous_veneration__pragmatic_incoherence_reading, powerless, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
