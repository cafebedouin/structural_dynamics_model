% ============================================================================
% CONSTRAINT STORY: derivative_work_statutory_boundary__coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_derivative_work_statutory_boundary__coordination_reading, []).

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
 *   constraint_id: derivative_work_statutory_boundary__coordination_reading
 *   human_readable: Derivative Work Boundary — Narrow Fixed-Recasting Reading (Coordination Reading)
 *   domain: intellectual_property/technological_governance/information_economics
 *
 * SUMMARY:
 *   This story authors the coordination_reading of the
 *   derivative_work_statutory_boundary kernel: the boundary sits at fixed
 *   recastings that substantially incorporate original expression, and
 *   everything short of that — transformative reinterpretation, intermediate
 *   copying, machine-learning ingestion — lies outside the permission
 *   requirement. Assessed by this reading's own lights, the standing
 *   arrangement is a low-extraction coordination scaffold for cumulative
 *   creation: it solves the collective-action problem of building on culture
 *   without a per-use bargaining thicket, its participants are net
 *   beneficiaries, and it suppresses no alternative practice except
 *   substitutional recasting, which is the conduct it exists to reserve. The
 *   colloquial label 'the derivative-work boundary' decomposes, per the
 *   epsilon-invariance principle, into three structurally distinct
 *   constraints — this narrow-boundary reading, the enclosure reading
 *   (universal ingestion liability, high extraction), and the hybrid
 *   carve-out reading (commerciality-contingent boundary administered through
 *   licensing markets) — each with its own epsilon, victim set, and
 *   classification, linked through network.affects_constraints. This file
 *   contains only the coordination reading; the sibling epsilons are authored
 *   in their own files and are not averaged or hedged here.
 *
 * KEY AGENTS:
 *   - - transformative_creators: Primary beneficiary (moderate/mobile) — practice exists only inside the permission-free zone
 *   - - ai_developers: Primary beneficiary (powerful/arbitrage) — ingestion and non-recasting outputs unlicensed; multiple exit levers
 *   - - technology_platforms: Secondary beneficiary (institutional/mobile) — hosts user transformation at scale without clearance
 *   - - original_authors: Dual-positioned beneficiary/payer (organized/constrained) — collects recasting licenses, bears uncompensated transformative and training use
 *   - - style_originating_artists: Primary target (moderate/trapped) — absorbed into corpora without recourse; weakest exit in the structure
 *   - - licensing_intermediaries: Secondary target (organized/constrained) — business model erodes as the open zone grows
 *   - - courts_legislatures: Agenda setter (institutional/analytical) — administers and could move the boundary
 *   - - general_public: Diffuse beneficiary (moderate/mobile) — consumes the enriched cultural environment
 *   - - ip_scholars: Analytical observer (analytical/analytical) — measures the tradeoff, collects nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_statutory_boundary__coordination_reading, 0.18).
domain_priors:suppression_score(derivative_work_statutory_boundary__coordination_reading, 0.35).
domain_priors:theater_ratio(derivative_work_statutory_boundary__coordination_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__coordination_reading, rope).
narrative_ontology:human_readable(derivative_work_statutory_boundary__coordination_reading, "Derivative Work Boundary — Narrow Fixed-Recasting Reading (Coordination Reading)").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__coordination_reading, "intellectual_property/technological_governance/information_economics").

domain_priors:requires_active_enforcement(derivative_work_statutory_boundary__coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__coordination_reading, '2b2985a3-3442-47f4-9867-54640b598659').
narrative_ontology:cs_kernel_codification('2b2985a3-3442-47f4-9867-54640b598659', fixed_text).
narrative_ontology:cs_authority_grounding('2b2985a3-3442-47f4-9867-54640b598659', lineage).
narrative_ontology:cs_interpretation_layer_present('2b2985a3-3442-47f4-9867-54640b598659').
narrative_ontology:cs_reading_relation('2b2985a3-3442-47f4-9867-54640b598659', derivative_work_statutory_boundary__enclosure_reading, forecloses).
narrative_ontology:cs_reading_relation('2b2985a3-3442-47f4-9867-54640b598659', derivative_work_statutory_boundary__hybrid_carveout_reading, forecloses).
narrative_ontology:cs_axiom('2b2985a3-3442-47f4-9867-54640b598659', foundational, only_fixed_substantial_recastings_require_license).
narrative_ontology:cs_axiom_status(only_fixed_substantial_recastings_require_license, holdable).
narrative_ontology:cs_axiom_grounding('2b2985a3-3442-47f4-9867-54640b598659', only_fixed_substantial_recastings_require_license, conventional).
narrative_ontology:cs_axiom('2b2985a3-3442-47f4-9867-54640b598659', foundational, transformative_intermediate_use_presumed_lawful).
narrative_ontology:cs_axiom_status(transformative_intermediate_use_presumed_lawful, holdable).
narrative_ontology:cs_axiom_grounding('2b2985a3-3442-47f4-9867-54640b598659', transformative_intermediate_use_presumed_lawful, instrumental).
narrative_ontology:cs_reference_frame('2b2985a3-3442-47f4-9867-54640b598659', narrow_transformation_permissive_boundary).
narrative_ontology:cs_drift_state('2b2985a3-3442-47f4-9867-54640b598659', generative_ai_litigation_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('2b2985a3-3442-47f4-9867-54640b598659', '').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__coordination_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, transformative_creators).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, ai_developers).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, technology_platforms).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, general_public).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, original_authors).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__coordination_reading, style_originating_artists).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__coordination_reading, licensing_intermediaries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__coordination_reading, original_authors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Make videos, music, fiction, and visual art that rework existing songs, films, characters, and images — parody, commentary, sampling, fan works, video essays. Their output is lawful without advance permission so long as it transforms rather than substitutes for the source. Their livelihood depends on the permission-free space remaining open; a shift to per-use licensing would price most of them out of the practice entirely.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, transformative_creators, beneficiary,
    moderate, biographical, mobile, global).

% Train large models on corpora containing copyrighted books, articles, images, and code, and ship products built on them. Under this reading no ex-ante license is required for ingestion or for outputs that do not recast a specific work. They hold levers most other seats lack: relocating training operations across jurisdictions, switching to licensed or synthetic data, or negotiating voluntary deals with the largest rights-holders.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, ai_developers, beneficiary,
    powerful, generational, arbitrage, global).

% Host user-generated remixes, covers, reaction video, and fan fiction at planetary scale. A narrow derivative-work boundary combined with notice-based enforcement lets them operate without clearing rights for every upload; a broader boundary would expose them to liability for their users' creativity and force pre-publication review of the entire feed.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, technology_platforms, beneficiary,
    institutional, generational, mobile, global).

% Writers, musicians, photographers, and the publishers, labels, and estates behind them. They keep exclusive control over translations, sequels, dramatizations, and other fixed recastings of their work and collect licensing income there. At the same time they receive nothing when their expression trains models, anchors criticism, or inspires stylistic imitation — they cannot opt out of others' transformative use of work already published, and their guilds and trade groups continuously lobby to widen the boundary.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, original_authors, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(derivative_work_statutory_boundary__coordination_reading, original_authors, payer).

% Illustrators and digital artists whose distinctive styles and back catalogs are absorbed into training corpora and imitated by generative tools. Past work cannot be recalled from datasets, style itself receives no protection, and individual artists lack the resources to sue — though class actions have begun to pool their claims. Compensation reaches them only through voluntary collective bargaining or litigation, not through the boundary itself.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, style_originating_artists, payer,
    moderate, biographical, trapped, global).

% Stock-photo agencies, clip-art houses, and reprint-permission services whose revenue is per-use licensing of expressive material. Every category of use moved into the permission-free zone shrinks their addressable market directly. Their business model cannot migrate into the open zone, so they advocate the widest possible boundary and fund litigation testing its edges.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, licensing_intermediaries, payer,
    organized, immediate, constrained, global).

% Federal courts draw and redraw the line case by case through the substantial-similarity, abstraction-filtration, and fair-use doctrines, and Congress can redefine the derivative-work right by statute. They administer the narrow placement this reading describes while openly acknowledging the question is unsettled for machine learning, and they absorb the contest between narrower and broader boundary placements.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, courts_legislatures, agenda_setter,
    institutional, generational, analytical, national).

% Readers, viewers, listeners, librarians, and library patrons. They inhabit a denser cultural environment — parodies, critiques, accessible translations, AI-assisted tools — than a permission-first regime would produce. Their interest is diffuse, exercised through market demand and fair-use advocacy organizations rather than through any seat at the table.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, general_public, beneficiary,
    moderate, civilizational, mobile, global).

% Legal academics and economists who map where the boundary sits, measure incentive and cumulative-creation effects on both sides, and testify in litigation and legislative hearings. They collect nothing from the arrangement and bear none of its costs.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, ip_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(derivative_work_statutory_boundary__coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(derivative_work_statutory_boundary__coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Defines, once and centrally, which acts of building on existing expression require the original author's consent: fixed recastings that substantially incorporate protected expression require authorization; transformative reinterpretations, intermediate copies, and machine-learning ingestion proceed without ex-ante negotiation. This converts an unmanageable thicket of per-use bargaining into a bright-line rule under which cumulative creation, criticism, and technology development can proceed.
% TRANSFER_FUNCTION: Transfers exclusive control over substitutional recastings from the public to original authors, and transfers freedom of transformative and intermediate use from authors to downstream creators, researchers, and technology builders — the latter transfer deliberately uncompensated.
% ABSENT_VOICES: Authors and estates who would price every transformative or training use are vocal in the wider policy debate but hold no seat inside this reading's framework; neither do training-data subjects whose personal images and texts circulate in corpora, nor creators of oral-tradition and folklore material that enters the commons without record or credit.
% DISAPPEARANCE_RATIONALE: If the boundary vanished overnight, substitutional recastings and unauthorized translations would flood markets within months, publisher investment in original works would reprice, platform user-generated ecosystems would lose their liability clarity, and the generative-AI sector's data practices would face either a permission thicket or a free-for-all — the cumulative-creation economy reorganizes around whichever norm rushes in first.
% FOUNDING_PROBLEM: How to grant authors enough exclusivity to finance creation — and to prevent mutilating or freeriding recastings of their work — without letting that exclusivity lock up the raw material from which later culture, criticism, scholarship, and technology are made.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: two centuries of Anglo-American case law from Folsom v. Marsh onward treats the incentive-versus-cumulation tradeoff as real; the Berne Convention's adaptation right presupposes it; advocates of the widest boundary concede the progress rationale while disputing its weight; and economic scholarship across camps documents both incentive effects and cumulative-creation externalities.
narrative_ontology:disappearance_verdict(derivative_work_statutory_boundary__coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(derivative_work_statutory_boundary__coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(derivative_work_statutory_boundary__coordination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(derivative_work_statutory_boundary__coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(derivative_work_statutory_boundary__coordination_reading, 0.18, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(derivative_work_statutory_boundary__coordination_reading_tests).
:- end_tests(derivative_work_statutory_boundary__coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is low (0.18) because the arrangement's costs are residual: uncompensated transformative and training use of published work, plus the litigation cost of policing the fixed-recasting line — far below what any licensing-regime reading would impose. Suppression (0.35) is real but bounded: notice-and-takedown volume, injunction practice, and adjacent anti-circumvention machinery ride the same enforcement apparatus, yet the design leaves the transformative zone open by construction, so no participant's alternative practice is blocked. Theater_ratio (0.15) reflects a functional doctrine with some formulaic four-factor ritual. Accessibility_collapse (0.45): the total-freedom alternative is closed for recastings, but the transformative zone stays wide and rival boundary placements remain politically live. Resistance (0.40) is continuous rights-holder litigation and lobbying to widen the line. Claim and metrics are independent authored facts: the rope claim states the structure I believe true (net-beneficiary participants, minimal coercive overhead, unsuppressed alternatives); the metrics state what I believe descriptively operative, including the enforcement thickening visible in the suppression series. Seat divergence: original_authors (beneficiary with payer secondary role) compute differently from pure beneficiaries; style_originating_artists, with trapped exit, sit nearest the target end and compute the harshest experience of the same boundary; ai_developers' arbitrage exit damps their effective burden. The mild oscillation in the extractiveness series (rise through codification, dip after the transformativeness clarification of the mid-1990s, renewed rise in the AI era) tracks doctrinal clarification cycles, not intermittent reinforcement. Gain_flow is authored 'diffuse' affirmatively: I checked every named seat and none captures the arrangement's modest extraction — litigation costs dissipate into the court system and the uncompensated-use burden is forgone revenue, not received gain; benefits spread across the beneficiary seats. Fixing_cost is 'prohibitive': redrawing or removing the boundary would strand reliance interests across the entire generative and remix economy relative to negligible benefit while the arrangement functions.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter seat the arrangement is a settled doctrinal instrument administered case by case; from the beneficiary seats it is the invisible precondition of an entire practice; from the trapped artist seat the same boundary reads as uncompensated appropriation with no exit; from the constrained intermediary seat it reads as steady market erosion. One line, four experiences — the engine computes this per-seat divergence from power, exit, and role; the authored claim does not adjudicate it. A further identity dynamic runs through the agenda_setter seat: the bench's interpretive practice has fused institutionally with its balancing methodology, so drift is acknowledged candidly in opinions while the underlying frame is defended by the same act of acknowledgment.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (transformative_creators, ai_developers, technology_platforms, general_public) drive those seats toward the beneficiary end of directionality; the victim declarations (style_originating_artists, licensing_intermediaries) drive them toward the target end, with the trapped exit of the artists pushing them nearer full-target than the constrained intermediaries. Original_authors carry a dual declaration — beneficiary of recasting control, bearer of uncompensated transformative and training use — placing them near the symmetric midpoint, which the derivation captures through the secondary_role rather than any override. Courts_legislatures occupy an administrative position near symmetry. No directionality overrides are used: the derivation from beneficiary/victim data plus exit options reproduces the structure faithfully, and an override keyed to a shared power atom (several seats share 'organized') would conflate seats with opposed positions. Suppression is authored as a raw structural property and is not scaled; only extractiveness is scaled by directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy: the founding problem — financing creation without enclosing the raw material of later culture — remains live and is intensified by generative AI. The classification discipline cuts in two directions here. Partisans of the widest boundary would read the whole arrangement as pure extraction; property absolutists would read the boundary as a natural entitlement of authorship. Reading-indexed epsilon over the fixed referent — the standing narrow-boundary arrangement, assessed by this reading's lights — keeps the low-extraction coordination core visible without denying the real costs borne by style-originating artists and licensing intermediaries. Were the founding problem ever to die (for instance, if creation financing decoupled from exclusivity entirely), continued enforcement of the boundary would become theatrical maintenance and the type would drift toward inertial persistence; the temporal series shows no such decay — theater_ratio stays below 0.2 across the whole interval while the coordination function remains load-bearing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is the coordination_reading of the derivative_work_statutory_boundary kernel; which reading of the kernel ultimately governs — this one, the enclosure_reading (any use of copyrighted expression in creating a new work is preparation of a derivative work), or the hybrid_carveout_reading (boundary varies by commercial exploitation)?',
    'Dispositive appellate rulings in the pending machine-learning training-data litigation, or congressional amendment of the derivative-work definition; the disagreement is located in the placement of the boundary itself — fixed-recasting-only versus any-ingestion versus commerciality-contingent.',
    'Enclosure adoption would raise epsilon sharply, expand the victim set to every transformer of expression and every model developer, and shift the classification strongly extractive; hybrid adoption would make the boundary administrable only through licensing markets, producing moderate epsilon with a new intermediary beneficiary class; retention of this reading preserves the low-extraction coordination profile.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which of three mutually exclusive boundary placements governs the derivative-work kernel.').

omega_variable(
    intermediate_copy_doctrinal_status,
    'Is freedom of intermediate copying (reverse-engineering buffers, training-time ingestion, format shifting) a stable feature of the boundary itself, or an artifact of fair-use case law that a future court could withdraw without touching the fixed-recasting definition?',
    'Appellate treatment of intermediate-copy claims that declines to reach fair use and addresses the derivative-work definition directly.',
    'If intermediate-use freedom is fair-use-dependent, this reading''s scope narrows substantially and effective extraction rises for the software and AI sectors; if it is definitional, the reading is robust to fair-use retrenchment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intermediate_copy_doctrinal_status, empirical, 'Whether intermediate-use freedom rests on the definition or on fair-use doctrine.').

omega_variable(
    substitution_vs_stimulation,
    'Does uncompensated transformative and machine-learning use of existing works substitute for demand for the originals (imposing real market harm on authors) or stimulate it (functioning as promotion)?',
    'Economic studies of displacement versus audience-building effects in markets exposed to transformative works and generative outputs, entered into the litigation record.',
    'Large substitution effects would mean the uncompensated-use burden on authors is a genuine cost of the arrangement rather than a definitional artifact, raising effective extraction and drifting the reading toward a hybrid coordination/extraction profile; dominant stimulation effects confirm the low-extraction assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_vs_stimulation, empirical, 'Whether uncompensated downstream use harms or feeds the market for originals.').

omega_variable(
    suppression_structural_vs_chilling,
    'Is the measured suppression structural (notice-and-takedown volume, injunction practice, anti-circumvention machinery riding the same enforcement apparatus) or internalized (creators abandoning lawful transformative projects out of litigation fear before any enforcement action occurs)?',
    'Post-adjudication suppression trajectory: if creator-side self-censorship persists after favorable rulings clarify the permission-free zone, a substantial share of suppression is internalized and travels with the targets regardless of the doctrine''s text.',
    'If internalized, effective suppression exceeds the structural measure — the boundary''s open zone is underused even where it formally protects users, and remedying the doctrine alone would not restore the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_chilling, empirical, 'Structural enforcement versus internalized chilling as the carrier of suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__coordination_reading, 0, 185).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dwsb_coord_tr_t0, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 0, 0.06).
narrative_ontology:measurement(dwsb_coord_tr_t30, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 30, 0.07).
narrative_ontology:measurement(dwsb_coord_tr_t60, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 60, 0.09).
narrative_ontology:measurement(dwsb_coord_tr_t90, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 90, 0.1).
narrative_ontology:measurement(dwsb_coord_tr_t120, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 120, 0.12).
narrative_ontology:measurement(dwsb_coord_tr_t150, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 150, 0.13).
narrative_ontology:measurement(dwsb_coord_tr_t185, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 185, 0.15).

% Extraction over time
narrative_ontology:measurement(dwsb_coord_be_t0, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(dwsb_coord_be_t30, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 30, 0.11).
narrative_ontology:measurement(dwsb_coord_be_t60, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 60, 0.13).
narrative_ontology:measurement(dwsb_coord_be_t90, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 90, 0.15).
narrative_ontology:measurement(dwsb_coord_be_t120, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 120, 0.16).
narrative_ontology:measurement(dwsb_coord_be_t150, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 150, 0.14).
narrative_ontology:measurement(dwsb_coord_be_t185, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 185, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(dwsb_coord_su_t0, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(dwsb_coord_su_t30, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 30, 0.12).
narrative_ontology:measurement(dwsb_coord_su_t60, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 60, 0.16).
narrative_ontology:measurement(dwsb_coord_su_t90, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 90, 0.2).
narrative_ontology:measurement(dwsb_coord_su_t120, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 120, 0.26).
narrative_ontology:measurement(dwsb_coord_su_t150, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 150, 0.33).
narrative_ontology:measurement(dwsb_coord_su_t185, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 185, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_work_statutory_boundary__coordination_reading, resource_allocation).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, derivative_work_statutory_boundary__enclosure_reading).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, derivative_work_statutory_boundary__hybrid_carveout_reading).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, fair_use_doctrine).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'the derivative-work boundary' covers three structurally distinct claims that must not share one story. This coordination reading (narrow fixed-recasting boundary, low epsilon, rope-shaped) is upstream in legitimacy terms — two centuries of fair-use lineage feed it. The enclosure reading (universal ingestion liability, high epsilon) draws rhetorical force from the same statutory text's breadth ('recast, transformed, or adapted'). The hybrid carve-out reading (commerciality-contingent boundary, moderate epsilon administered through licensing markets) borrows from both. Edges link the family for contamination propagation: a doctrinal shift in any member re-prices the other two, and the pending machine-learning litigation is the live transmission channel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
