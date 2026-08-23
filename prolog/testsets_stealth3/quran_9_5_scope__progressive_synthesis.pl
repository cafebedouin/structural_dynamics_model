% ============================================================================
% CONSTRAINT STORY: quran_9_5_scope__progressive_synthesis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_9_5_scope__progressive_synthesis, []).

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
 *   constraint_id: quran_9_5_scope__progressive_synthesis
 *   human_readable: Verse 9:5 Progressive-Synthesis Settlement (Time-Bound Directive Reading)
 *   domain: religious/hermeneutical/political-theological
 *
 * SUMMARY:
 *   Verse 9:5 — the so-called Sword Verse — is a contested kernel in Islamic
 *   political theology: its scope decides whether a standing universal
 *   offensive-warfare obligation exists, a conditional defensive doctrine
 *   exists, or no live directive exists at all. This story instantiates ONE
 *   reading of that kernel, the progressive synthesis: the verse's directive
 *   is a time-bound seventh-century political directive, not an eternal legal
 *   command, and the Quranic ethical trajectory supersedes literalist
 *   application — under this reading the verse exits active constraint space,
 *   binding neither the polytheists it historically addressed nor any later
 *   actor. The constraint classified here is the settlement that reading
 *   operates where it prevails: an interpretive regime that coordinates the
 *   modern tradition's self-understanding (resolving the crisis the
 *   literalist reading creates for pluralist citizenship and minority civic
 *   standing) while, through that same structure, transferring interpretive
 *   capital — curricular control, state religious appointments,
 *   counter-extremism legitimation — away from textualist authority
 *   structures whose warrant is the verse's ongoing binding force. Per the
 *   ε-invariance principle, the colloquial label 'the Sword Verse problem'
 *   decomposes into three structurally distinct constraints, one per reading,
 *   with different ε and different victim sets: the abrogating_universal
 *   reading (a maximal live constraint with maximal extraction) and the
 *   contextual_defensive reading (a conditionally live constraint) are
 *   separate stories linked via network.affects_constraints. The claimed_type
 *   and the metrics below are authored independently: the claim states what
 *   this authoring seat believes is structurally true of the settlement; the
 *   metrics describe its actual operation as the record shows it.
 *
 * KEY AGENTS:
 *   - modernist_interpretive_institutions: agenda-setter and principal beneficiary (institutional/identity_locked) — administers the settlement and receives the transferred interpretive capital
 *   - textualist_authority_structures: primary target (organized/identity_locked) — bears the stripping of the verse's warrant
 *   - jihadi_recruitment_networks: secondary target (organized/identity_locked) — loses the proof-text; subsidized by the contest itself
 *   - muslim_minority_communities: beneficiary with payer costs (organized/constrained) — receives civic protection, pays intra-communal suspicion and hermeneutical labor
 *   - secular_pluralist_polities: beneficiary (institutional/mobile) — receives the pluralism-compatible settlement at near-zero cost
 *   - targeted_non_muslim_communities: excluded (powerless/trapped) — the people the verse is wielded against, with no seat in the contest that decides its scope
 *   - academic_islamic_studies: analytical observer (institutional/analytical) — documents the contest, adjudicates nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_9_5_scope__progressive_synthesis, 0.4).
domain_priors:suppression_score(quran_9_5_scope__progressive_synthesis, 0.62).
domain_priors:theater_ratio(quran_9_5_scope__progressive_synthesis, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, extractiveness, 0.4).
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_9_5_scope__progressive_synthesis, tangled_rope).
narrative_ontology:human_readable(quran_9_5_scope__progressive_synthesis, "Verse 9:5 Progressive-Synthesis Settlement (Time-Bound Directive Reading)").
narrative_ontology:topic_domain(quran_9_5_scope__progressive_synthesis, "religious/hermeneutical/political-theological").

domain_priors:requires_active_enforcement(quran_9_5_scope__progressive_synthesis).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_9_5_scope__progressive_synthesis, '4203c6eb-229b-48bb-acdf-1e207781e601').
narrative_ontology:cs_kernel_codification('4203c6eb-229b-48bb-acdf-1e207781e601', fixed_text).
narrative_ontology:cs_authority_grounding('4203c6eb-229b-48bb-acdf-1e207781e601', lineage).
narrative_ontology:cs_interpretation_layer_present('4203c6eb-229b-48bb-acdf-1e207781e601').
narrative_ontology:cs_reading_relation('4203c6eb-229b-48bb-acdf-1e207781e601', quran_9_5_scope__abrogating_universal, forecloses).
narrative_ontology:cs_reading_relation('4203c6eb-229b-48bb-acdf-1e207781e601', quran_9_5_scope__contextual_defensive, influences).
narrative_ontology:cs_axiom('4203c6eb-229b-48bb-acdf-1e207781e601', foundational, ethical_trajectory_supersedes_literalism).
narrative_ontology:cs_axiom_status(ethical_trajectory_supersedes_literalism, holdable).
narrative_ontology:cs_axiom_grounding('4203c6eb-229b-48bb-acdf-1e207781e601', ethical_trajectory_supersedes_literalism, theological).
narrative_ontology:cs_axiom('4203c6eb-229b-48bb-acdf-1e207781e601', foundational, warfare_directives_historically_indexed_not_timeless).
narrative_ontology:cs_axiom_status(warfare_directives_historically_indexed_not_timeless, holdable).
narrative_ontology:cs_axiom_grounding('4203c6eb-229b-48bb-acdf-1e207781e601', warfare_directives_historically_indexed_not_timeless, empirically_contingent).
narrative_ontology:cs_reference_frame('4203c6eb-229b-48bb-acdf-1e207781e601', ethical_trajectory_supersession_frame).
narrative_ontology:cs_drift_state('4203c6eb-229b-48bb-acdf-1e207781e601', contemporary_textualist_resurgence, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('4203c6eb-229b-48bb-acdf-1e207781e601', '').
narrative_ontology:cs_kernel_id(quran_9_5_scope__progressive_synthesis, quran_9_5_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_9_5_scope__progressive_synthesis, secular_pluralist_polities).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__progressive_synthesis, muslim_minority_communities).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__progressive_synthesis, modernist_interpretive_institutions).
narrative_ontology:constraint_victim(quran_9_5_scope__progressive_synthesis, textualist_authority_structures).
narrative_ontology:constraint_victim(quran_9_5_scope__progressive_synthesis, jihadi_recruitment_networks).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(quran_9_5_scope__progressive_synthesis, muslim_minority_communities).
narrative_ontology:constraint_vindicates(quran_9_5_scope__progressive_synthesis, no_compulsion_in_religion_principle).
narrative_ontology:constraint_vindicates(quran_9_5_scope__progressive_synthesis, occasional_revelation_hermeneutic).
narrative_ontology:constraint_vindicates(quran_9_5_scope__progressive_synthesis, ethical_trajectory_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% State religious directorates, reformed university faculties, and the modernist line of the great teaching institutions administer the settlement: they certify which applications of the verse are legitimate, train clergy in the contextual-ethical method, and staff the counter-extremism apparatus that de-platforms rival readings. They receive the curricular appointments, funding, and legitimation the settlement distributes. Their institutional identity is fused with the settlement — reversing it would dissolve what these institutions have become.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, modernist_interpretive_institutions, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(quran_9_5_scope__progressive_synthesis, modernist_interpretive_institutions, beneficiary).

% States governing religiously diverse populations on pluralist terms gain an intra-traditional anchor: the settlement defuses the verse's use as a warrant for adversarial political theology and gives Muslim-majority and Muslim-minority polities alike a settled basis for equal citizenship. They invest in the settlement's institutional carriers through counter-extremism partnership and chaplaincy certification but bear little of its cost; their exit is cheap, since they could accommodate rival readings by security means instead.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, secular_pluralist_polities, beneficiary,
    institutional, generational, mobile, global).

% Muslims living as minorities in non-Muslim-majority polities receive the settlement's civic protection: the verse cannot be wielded to frame their presence as a garrison's, and their civic loyalty is hermeneutically secured. They pay intra-communally — suspicion from textualist neighbors, takfir-adjacent accusations of capitulation, and the hermeneutical labor of defending the reading without the institutional backing its administrators enjoy. Exit from the community that carries their identity is not realistically available.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, muslim_minority_communities, beneficiary,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(quran_9_5_scope__progressive_synthesis, muslim_minority_communities, payer).

% Madhhab-textualist councils and Salafi scholarly networks claim the verse's ongoing binding force and draw their authority from that claim. Where the settlement prevails they are excluded from state religious appointments, their curricula are displaced, and their central proof-text is de-legitimized. They cannot abandon the claim without dissolving the authority it constitutes — the literalist warrant is not an asset they hold but the substance of who they are.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, textualist_authority_structures, payer,
    organized, generational, identity_locked, global).

% Militant networks wield the abrogating reading as their central recruitment and doctrinal proof-text. The settlement strips that proof-text's legitimacy in every institutional arena they might otherwise contest, and the counter-extremism apparatus built on the settlement de-platforms and criminalizes them. The contest itself, however, subsidizes them: each institutional de-legitimation confirms their narrative of a corrupted establishment, and enforcement validates the persecution theology their literalist reading anchors.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, jihadi_recruitment_networks, payer,
    organized, biographical, identity_locked, global).

% Non-Muslim minorities in regions where jihadi proof-texting operates — the people the verse's literalist reading is wielded against — have no seat in the hermeneutical contest that decides the verse's scope. The contest is adjudicated entirely within Islamic authority structures; they can appeal for the settlement's success but cannot vote on it, and their safety depends on an argument they are not party to.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, targeted_non_muslim_communities, excluded,
    powerless, biographical, trapped, continental).

% The scholarly field documents the contest: the genealogy of the abrogation doctrine, the occasion-of-revelation reports, the history of the modernist turn. It adjudicates nothing institutionally but supplies the evidentiary base both sides claim, and its own legitimacy depends on not being captured by either seat.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, academic_islamic_studies, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_9_5_scope__progressive_synthesis, modernist_interpretive_institutions).
narrative_ontology:fixing_cost_class(quran_9_5_scope__progressive_synthesis, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the tradition's hermeneutical crisis: it reconciles the verse's literal text with the Quran's ethical arc, giving modern Muslim-majority polities, Muslim minorities, and the tradition's institutions a settled, internally grounded basis for pluralist citizenship, non-adversarial relations with non-Muslims, and a defensible boundary between legitimate and illegitimate readings of the warfare verses.
% TRANSFER_FUNCTION: Moves interpretive authority over the verse — and the institutional capital riding on it (curricular control, state religious appointments, counter-extremism funding and legitimation, the warrant to speak for the tradition on war and peace) — from textualist authority structures to modernist-pluralist interpretive institutions and the secular-pluralist frameworks those institutions anchor.
% ABSENT_VOICES: The non-Muslim communities the verse's literalist reading is wielded against — historically the treaty-breaking tribes of the verse's occasion, today the minorities targeted by jihadi proof-texting — have no seat in the contest: the verse's scope is adjudicated entirely within Islamic authority structures, and those it is wielded against can only appeal for the settlement's success. The pre-modern jurists who built the abrogationist consensus cannot answer. Dissenting textualists are present in the discourse but are frequently anathematized rather than engaged.
% DISAPPEARANCE_RATIONALE: If the settlement vanished overnight, the contest would reorganize around abrogating_universal versus contextual_defensive with no deflationary third pole: state counter-extremism frameworks would lose their intra-traditional anchor and fall back on purely coercive suppression, jihadi proof-texting would regain an uncontested textual claim, Muslim-minority civic settlements would lose their hermeneutical foundation, and the modernist institutions' identity and mandate would dissolve. The arrangements of every named seat depend on the settlement's standing.
% FOUNDING_PROBLEM: The classical abrogationist settlement — verse 9:5 read as superseding the peaceful verses and universalized by the classical jurisprudence of jihad — made the tradition's political theology structurally adversarial to non-Muslim polities. As Muslims became permanent minorities inside pluralist states and Muslim states entered the nation-state system, that inheritance became unbearably costly; the modernist reformers built the progressive synthesis to solve the crisis of coherence between the text's warfare verses and the tradition's ethical core and civic circumstances.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: jihadi primary sources themselves attest the literalist reading's live operational force by citing the verse as the 'verse of the sword' in doctrine and recruitment; academic scholarship on the abrogation doctrine (the nasikh-mansukh apparatus and its genealogy) documents the classical settlement's structure independently of any modernist apologetic; security-services assessments of extremist recruitment narratives attest the proof-text's ongoing use. The textualist authorities' own anathematization of modernism corroborates the crisis from the opposing seat. No seat inside the settlement's beneficiary set is relied upon for the status claim.
narrative_ontology:disappearance_verdict(quran_9_5_scope__progressive_synthesis, world_rearranges).
narrative_ontology:founding_problem_status(quran_9_5_scope__progressive_synthesis, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_9_5_scope__progressive_synthesis, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quran_9_5_scope__progressive_synthesis, 'none', 1).
narrative_ontology:epsilon_provenance(quran_9_5_scope__progressive_synthesis, 0.4, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_9_5_scope__progressive_synthesis_tests).
:- end_tests(quran_9_5_scope__progressive_synthesis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.40: the settlement's operation transfers real but non-material capital — interpretive authority, institutional positions, the verse's warrant — from textualist structures to the modernist-pluralist complex; the transfer is substantial where the settlement is institutionally entrenched and marginal where it wins by argument alone. Suppression 0.62: the settlement's institutional hold requires active enforcement because its principal alternative is not a strawman but a live, resurgent, well-resourced reading; state religious apparatuses, curricular control, and counter-extremism criminalization carry that enforcement. Theater 0.42 and rising across the interval: as the settlement became a state asset and then a counter-narrative industry, the performative share of its activity (declarations, conferences, counter-narrative production) grew against a functional hermeneutical core that remains real. Accessibility_collapse 0.25: the settlement forecloses nothing hermeneutically — both sibling readings remain fully live and held; alternatives do not collapse. Resistance 0.70: the settlement meets takfir accusation, the exile of modernist scholars, jihadi anathematization, and sustained textualist institutional resistance. The measurement series run on one shared time grid, with all three metrics authored at all seven points. The late-interval ebb in extractiveness (0.44 to 0.40) and enforcement (0.66 to 0.62) reflects the contest's decentralization online: institutional stripping yield plateaus as textualist capital reconstitutes outside state reach while enforcement normalizes slightly.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and should. From the modernist-institutional seat the settlement is a rope: coordination it built, staffs, and needs. From the textualist seat the same structure computes as enforced extraction: their central warrant is stripped by a settlement they never joined, and their exit is identity-locked because the literalist claim is not an asset they hold but the substance of their authority. From the jihadi seat the structure extracts and subsidizes at once: de-platforming confirms the corruption narrative that recruits. From the pluralist-beneficiary seat it is again a rope, purchased at near-zero cost. The reading's self-description — corrective, not extractive — is itself part of the gap: what its carriers call restoring the text's meaning, the payer seats experience as dispossession of their warrant. The payer seats also form a loose de facto coalition (textualist-jihadi convergence against the settlement), which raises enforcement costs and explains why the suppression series plateaus late in the interval rather than continuing to climb. Identity fusion runs on both sides: the madhhab apparatus and Salafi scholarly identity have become the literalist warrant, and the post-reform modernist institutions have become the settlement — if either frame broke, the extraction half of the hybrid would collapse and the story would drift toward pure rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit near the beneficiary end of directionality: secular_pluralist_polities (d near 0.05) receive the pluralism-compatible settlement while bearing almost none of its cost and holding cheap exit; modernist_interpretive_institutions (d near 0.10) both administer the settlement and receive the transferred capital; muslim_minority_communities (d near 0.25) receive civic protection but pay intra-communal suspicion and hermeneutical labor, with constrained exit. Targets sit near the target end: textualist_authority_structures (d near 0.90) bear the stripping with identity-locked exit; jihadi_recruitment_networks (d near 0.75) are structurally targeted but partially subsidized by the contest itself, which their persecution narrative converts into recruitment fuel. No directionality_overrides are authored: the override mechanism keys on power atoms, and this story's organized-atom seats diverge in directionality for reasons the beneficiary/victim declarations and exit attributions already encode — a power-atom-level override would mis-key across seats. The jihadi contest-subsidy nuance is carried in that seat's situation text instead.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — the tradition's crisis of coherence under modernity, with the abrogationist settlement making its political theology structurally adversarial to pluralist order — is live, and the settlement's function is therefore operative: this is not a mandatrophy case, and the R5 fields cohere (status live, verdict world_rearranges, no mismatch flag). The mandatrophy risk runs forward, not backward: if the abrogating reading were ever fully defeated, the settlement's enforcement machinery would outlive its function and drift toward theatrical maintenance of a victory against a vanishing enemy — the rising theater_ratio series is the leading indicator to watch, and the piton boundary sits where theater crosses functional maintenance. The classification prevents the two symmetric mislabelings: reading the settlement as pure rope would hide the enforced capital transfer its payer seats experience; reading it as pure snare would hide the genuine coordination function — the hermeneutical crisis is real, the pluralist-citizenship problem is real, and the settlement solves them — that justifies its coordination half.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This story instantiates one reading (progressive_synthesis) of the kernel quran_9_5_scope; what structural delta would obtain under each sibling reading''s prevalence, and where exactly do the readings disagree?',
    'The sibling stories (quran_9_5_scope__abrogating_universal, quran_9_5_scope__contextual_defensive) are authored separately under the same kernel; cross-reading comparison runs through the kernel''s reading set, not through this story''s metrics.',
    'If abrogating_universal prevailed, the verse re-enters active constraint space at maximal extraction (standing universal offensive-warfare obligation; victims include all non-submitted non-Muslim populations). If contextual_defensive prevailed, the verse stays conditionally live (defensive and treaty contexts only). The disagreement is located in the verse''s temporal scope and the abrogation doctrine''s reach — the specific structural element this reading''s time-bound scoping denies.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of a three-reading kernel; sibling deltas and the disagreement''s location.').

omega_variable(
    faithful_recovery_vs_modern_imposition,
    'Is the progressive synthesis a faithful recovery of the Quran''s own ethical trajectory, or a modern imposition that subordinates textual integrity to liberal-pluralist legitimacy?',
    'Intra-textual analysis testing whether the trajectory claim coheres across the corpus (the Meccan-Medinan arc, 2:256 against 9:5''s occasion-of-revelation reports) versus genealogical analysis testing whether the reading tracks modernity''s pressures rather than the text''s structure.',
    'If imposition, the settlement''s coordination function is cover and its capital transfer is closer to pure extraction — classification drifts toward snare from the textualist seat. If faithful recovery, the coordination function is deep and the tangled_rope hybrid is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(faithful_recovery_vs_modern_imposition, conceptual, 'Whether the reading''s ethical-trajectory warrant is internally grounded or externally driven.').

omega_variable(
    contest_loss_or_extraction,
    'Is the cost the settlement imposes on textualist authority structures a legitimate contest loss (de-legitimation of a claim through open argument) or extraction (transfer of interpretive capital to a rival elite through institutional power)?',
    'Compare the settlement''s operation across contexts: where it prevails by argument (academic and convictional arenas) versus where it prevails by enforcement (state religious appointments, criminalization of jihadi discourse, curricular exclusion). If the capital transfer tracks enforcement intensity rather than argumentative merit, extraction dominates the hybrid.',
    'Resolves whether the textualist seat computes as payer-within-coordination or victim-of-extraction; a strongly enforcement-tracked transfer pushes the whole story toward the snare boundary of tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contest_loss_or_extraction, empirical, 'Whether the settlement''s stripping of textualist capital is contest loss or enforced extraction.').

omega_variable(
    enforcement_sovereignty_confusion,
    'Is the measured suppression the settlement''s own enforcement requirement, or its state carriers'' interest in religious control wearing the settlement''s name?',
    'Separate the settlement''s intrinsic enforcement needs from carrier-state interests: examine the settlement''s fate in jurisdictions where states withdraw enforcement — if it persists convictionally at scale, the measured suppression over-attributes state control to the settlement itself.',
    'If suppression is mostly carrier-state, the settlement-as-constraint is less coercive than measured and drifts toward rope; if intrinsic, tangled_rope holds with the enforcement series as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_sovereignty_confusion, empirical, 'Attribution of the settlement''s enforcement burden between the reading itself and its state carriers.').

omega_variable(
    textualist_identity_lock_mechanism,
    'What binds textualist authority structures to the contest — professional identity (career path dependence on the literalist warrant), institutional identity (the madhhab apparatus has become its function), or ideological identity (a worldview that makes revision unthinkable)?',
    'Post-defection trajectories of individual textualist scholars who accept the progressive synthesis: if they retain standing and community, the lock is professional or institutional and breakable; if defection costs everything, the lock is ideological.',
    'If the lock broke at scale, the settlement would have no one to strip — extraction collapses and the story drifts toward pure rope; the identity-locked exit attribution driving the textualist seat''s high directionality would be mis-specified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(textualist_identity_lock_mechanism, empirical, 'The fusion mechanism behind textualist exit-lock and its classification consequence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_9_5_scope__progressive_synthesis, 0, 125).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_9_5_scope__progressive_synthesis, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(qura_tr_t0, observed).
narrative_ontology:measurement(qura_tr_t20, quran_9_5_scope__progressive_synthesis, theater_ratio, 20, 0.12).
narrative_ontology:measurement_basis(qura_tr_t20, observed).
narrative_ontology:measurement(qura_tr_t40, quran_9_5_scope__progressive_synthesis, theater_ratio, 40, 0.18).
narrative_ontology:measurement_basis(qura_tr_t40, observed).
narrative_ontology:measurement(qura_tr_t60, quran_9_5_scope__progressive_synthesis, theater_ratio, 60, 0.25).
narrative_ontology:measurement_basis(qura_tr_t60, observed).
narrative_ontology:measurement(qura_tr_t80, quran_9_5_scope__progressive_synthesis, theater_ratio, 80, 0.3).
narrative_ontology:measurement_basis(qura_tr_t80, observed).
narrative_ontology:measurement(qura_tr_t100, quran_9_5_scope__progressive_synthesis, theater_ratio, 100, 0.38).
narrative_ontology:measurement_basis(qura_tr_t100, observed).
narrative_ontology:measurement(qura_tr_t125, quran_9_5_scope__progressive_synthesis, theater_ratio, 125, 0.42).
narrative_ontology:measurement_basis(qura_tr_t125, observed).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_9_5_scope__progressive_synthesis, base_extractiveness, 0, 0.08).
narrative_ontology:measurement_basis(qura_be_t0, observed).
narrative_ontology:measurement(qura_be_t20, quran_9_5_scope__progressive_synthesis, base_extractiveness, 20, 0.14).
narrative_ontology:measurement_basis(qura_be_t20, observed).
narrative_ontology:measurement(qura_be_t40, quran_9_5_scope__progressive_synthesis, base_extractiveness, 40, 0.24).
narrative_ontology:measurement_basis(qura_be_t40, observed).
narrative_ontology:measurement(qura_be_t60, quran_9_5_scope__progressive_synthesis, base_extractiveness, 60, 0.36).
narrative_ontology:measurement_basis(qura_be_t60, observed).
narrative_ontology:measurement(qura_be_t80, quran_9_5_scope__progressive_synthesis, base_extractiveness, 80, 0.42).
narrative_ontology:measurement_basis(qura_be_t80, observed).
narrative_ontology:measurement(qura_be_t100, quran_9_5_scope__progressive_synthesis, base_extractiveness, 100, 0.44).
narrative_ontology:measurement_basis(qura_be_t100, observed).
narrative_ontology:measurement(qura_be_t125, quran_9_5_scope__progressive_synthesis, base_extractiveness, 125, 0.4).
narrative_ontology:measurement_basis(qura_be_t125, observed).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_9_5_scope__progressive_synthesis, suppression_requirement, 0, 0.05).
narrative_ontology:measurement_basis(qura_su_t0, observed).
narrative_ontology:measurement(qura_su_t20, quran_9_5_scope__progressive_synthesis, suppression_requirement, 20, 0.1).
narrative_ontology:measurement_basis(qura_su_t20, observed).
narrative_ontology:measurement(qura_su_t40, quran_9_5_scope__progressive_synthesis, suppression_requirement, 40, 0.22).
narrative_ontology:measurement_basis(qura_su_t40, observed).
narrative_ontology:measurement(qura_su_t60, quran_9_5_scope__progressive_synthesis, suppression_requirement, 60, 0.45).
narrative_ontology:measurement_basis(qura_su_t60, observed).
narrative_ontology:measurement(qura_su_t80, quran_9_5_scope__progressive_synthesis, suppression_requirement, 80, 0.58).
narrative_ontology:measurement_basis(qura_su_t80, observed).
narrative_ontology:measurement(qura_su_t100, quran_9_5_scope__progressive_synthesis, suppression_requirement, 100, 0.66).
narrative_ontology:measurement_basis(qura_su_t100, observed).
narrative_ontology:measurement(qura_su_t125, quran_9_5_scope__progressive_synthesis, suppression_requirement, 125, 0.62).
narrative_ontology:measurement_basis(qura_su_t125, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_9_5_scope__progressive_synthesis, identity_coordination).
narrative_ontology:affects_constraint(quran_9_5_scope__progressive_synthesis, quran_9_5_scope__abrogating_universal).
narrative_ontology:affects_constraint(quran_9_5_scope__progressive_synthesis, quran_9_5_scope__contextual_defensive).

% DUAL FORMULATION NOTE:
% Kernel decomposition note: the colloquial label 'the Sword Verse problem' covers three structurally distinct claims with different ε, different victim sets, and different classifications. This story (progressive_synthesis: the verse exits active constraint space; the settlement's extraction is interpretive-capital transfer with ε ≈ 0.40; victims are the textualist authority structures) links to its siblings: quran_9_5_scope__abrogating_universal (verse maximally live; standing universal offensive-warfare obligation; maximal extraction from non-submitted populations) and quran_9_5_scope__contextual_defensive (verse conditionally live; defensive and treaty scope only; extraction concentrated on treaty-breaking and belligerent parties). The upstream sibling (contextual_defensive) supplies the methodological ground — occasionalist contextualism — that this reading radicalizes; the abrogating_universal reading is the alternative this settlement's enforcement machinery exists to hold off.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
