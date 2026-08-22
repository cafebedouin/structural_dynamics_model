% ============================================================================
% CONSTRAINT STORY: nicene_creed_authority__strict_orthodox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_creed_authority__strict_orthodox_reading, []).

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
 *   constraint_id: nicene_creed_authority__strict_orthodox_reading
 *   human_readable: Nicene Creed as Binding Metaphysical Ontology (Strict Orthodox Reading)
 *   domain: religious/ecclesial
 *
 * SUMMARY:
 *   Under the strict orthodox reading, the Nicene Creed is not a summary of
 *   faith but a binding metaphysical contract: every believer owes assent to
 *   one defined ontology of God, and deviation is not error but heresy, an
 *   offense warranting sanction. The arrangement couples a genuine unifying
 *   achievement — a shared rule of faith that held a dispersed communion
 *   together across languages, cultures, and centuries — to an enforcement
 *   apparatus that adjudicates assent, polices interpretation, and imposes
 *   penalties ranging from exclusion from communion to, in earlier centuries,
 *   referral to civil courts. The claim and the metrics are authored
 *   independently: the enforcing communions claim the arrangement as faithful
 *   guardianship of revealed truth, while the authored metrics describe
 *   substantially extractive, actively enforced operation with identifiable
 *   beneficiary and victim seats. FAMILY NOTE: this story is one member of a
 *   three-story constraint family decomposing the colloquial label 'creedal
 *   authority.' The strict reading instantiates a
 *   binding-assent-plus-sanction constraint (this file, epsilon 0.64); the
 *   symbolic_confessional_reading instantiates a historically contingent
 *   witness whose authority derives from community discernment (no
 *   heretic-victim set); the liturgical_habituation_reading instantiates an
 *   identity-boundary marker operating through performance independent of
 *   cognitive assent (victims reduce to those excluded from performance). The
 *   readings share a fixed text but differ in what compliance consists in,
 *   who adjudicates, and who bears cost — hence separate files, separate
 *   epsilon values, linked by network.affects_constraints.
 *
 * KEY AGENTS:
 *   - hierarchical_clergy: Primary beneficiary and agenda-setter (institutional/identity_locked) — adjudicates orthodoxy, administers sanction, collects interpretive authority
 *   - heterodox_communities: Primary target (powerless/trapped) — bears the heaviest sanctions, historically up to exile and death
 *   - lay_interpreters: Secondary target (moderate/identity_locked) — bears censure for unauthorized reading
 *   - ordinary_believers: Dual-positioned (moderate/constrained) — receives doctrinal clarity and communal identity, pays the obligation of assent
 *   - secular_rulers: Historical enforcer-beneficiary (powerful/mobile) — supplied the civil arm in exchange for a uniform cult
 *   - rival_theological_schools: Excluded counterparty (organized/trapped) — held rival articulations but never sat in the defining conversation
 *   - historians_of_doctrine: Analytical observer (analytical/analytical) — attests the genealogy from outside the benefiting parties
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_creed_authority__strict_orthodox_reading, 0.64).
domain_priors:suppression_score(nicene_creed_authority__strict_orthodox_reading, 0.55).
domain_priors:theater_ratio(nicene_creed_authority__strict_orthodox_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, extractiveness, 0.64).
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_creed_authority__strict_orthodox_reading, tangled_rope).
narrative_ontology:human_readable(nicene_creed_authority__strict_orthodox_reading, "Nicene Creed as Binding Metaphysical Ontology (Strict Orthodox Reading)").
narrative_ontology:topic_domain(nicene_creed_authority__strict_orthodox_reading, "religious/ecclesial").

domain_priors:requires_active_enforcement(nicene_creed_authority__strict_orthodox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_creed_authority__strict_orthodox_reading, 'bf3d05d8-81bd-489a-8861-108a77b740f1').
narrative_ontology:cs_kernel_codification('bf3d05d8-81bd-489a-8861-108a77b740f1', fixed_text).
narrative_ontology:cs_authority_grounding('bf3d05d8-81bd-489a-8861-108a77b740f1', lineage).
narrative_ontology:cs_interpretation_layer_present('bf3d05d8-81bd-489a-8861-108a77b740f1').
narrative_ontology:cs_reading_relation('bf3d05d8-81bd-489a-8861-108a77b740f1', nicene_creed_authority__symbolic_confessional_reading, forecloses).
narrative_ontology:cs_reading_relation('bf3d05d8-81bd-489a-8861-108a77b740f1', nicene_creed_authority__liturgical_habituation_reading, forecloses).
narrative_ontology:cs_axiom('bf3d05d8-81bd-489a-8861-108a77b740f1', foundational, creed_binds_all_to_one_metaphysical_ontology).
narrative_ontology:cs_axiom_status(creed_binds_all_to_one_metaphysical_ontology, holdable).
narrative_ontology:cs_axiom_grounding('bf3d05d8-81bd-489a-8861-108a77b740f1', creed_binds_all_to_one_metaphysical_ontology, deontological).
narrative_ontology:cs_axiom('bf3d05d8-81bd-489a-8861-108a77b740f1', foundational, metaphysical_deviation_warrants_sanction).
narrative_ontology:cs_axiom_status(metaphysical_deviation_warrants_sanction, holdable).
narrative_ontology:cs_axiom_grounding('bf3d05d8-81bd-489a-8861-108a77b740f1', metaphysical_deviation_warrants_sanction, instrumental).
narrative_ontology:cs_axiom('bf3d05d8-81bd-489a-8861-108a77b740f1', secondary, conciliar_definition_irreformable).
narrative_ontology:cs_axiom_status(conciliar_definition_irreformable, holdable).
narrative_ontology:cs_axiom_grounding('bf3d05d8-81bd-489a-8861-108a77b740f1', conciliar_definition_irreformable, conventional).
narrative_ontology:cs_reference_frame('bf3d05d8-81bd-489a-8861-108a77b740f1', apostolic_nicene_consensus).
narrative_ontology:cs_drift_state('bf3d05d8-81bd-489a-8861-108a77b740f1', contemporary_pluralist_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bf3d05d8-81bd-489a-8861-108a77b740f1', '').
narrative_ontology:cs_kernel_id(nicene_creed_authority__strict_orthodox_reading, nicene_creed_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_creed_authority__strict_orthodox_reading, hierarchical_clergy).
narrative_ontology:constraint_victim(nicene_creed_authority__strict_orthodox_reading, heterodox_communities).
narrative_ontology:constraint_victim(nicene_creed_authority__strict_orthodox_reading, lay_interpreters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__strict_orthodox_reading, ordinary_believers).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__strict_orthodox_reading, secular_rulers).
narrative_ontology:constraint_victim(nicene_creed_authority__strict_orthodox_reading, ordinary_believers).
narrative_ontology:constraint_vindicates(nicene_creed_authority__strict_orthodox_reading, nicene_homoousion_doctrine).
narrative_ontology:constraint_vindicates(nicene_creed_authority__strict_orthodox_reading, conciliar_authority_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convenes councils, issues definitions, and administers penalties for doctrinal deviation. Ordination ties each bishop's office to the unbroken chain that authorizes adjudication; a bishop who ceased enforcing the binding text would forfeit the office's meaning rather than merely change a policy. Instruments range from exclusion from communion to, in earlier centuries, referral of obstinate cases to civil courts. The office collects deference, jurisdiction, and the final word on what the tradition means.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, hierarchical_clergy, agenda_setter,
    institutional, generational, identity_locked, global).

% Hold christological or trinitarian positions the defined text rules out — communities descended from or analogous to Arians, Nestorians, Monophysites, and later radical reformers. Within territories where the reading governs, their options were conversion, concealment, exile, or penalty; several historical communities survived only outside the enforcement perimeter. Property, employment, and burial tracked conformity in the eras of fullest enforcement.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, heterodox_communities, payer,
    powerless, biographical, trapped, regional).

% Read scripture and theology without ordination or faculty appointment. Vernacular translation and independent commentary were historically restricted; a lay reader reaching conclusions contrary to the defined ontology risks censure, loss of communion, or exclusion from the congregation that constitutes their primary community. Leaving the tradition means leaving family, parish, and formed self-conception at once.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, lay_interpreters, payer,
    moderate, biographical, identity_locked, national).

% Recite the creed weekly, entrust children to catechesis, and order rites of passage around the shared profession. They receive a determinate identity and a trans-local community that recognizes them anywhere the profession runs. They also carry the obligation of assent and the exposure to sanction should a privately held conclusion become public.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, ordinary_believers, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(nicene_creed_authority__strict_orthodox_reading, ordinary_believers, payer).

% From Constantine onward, emperors and princes supplied the civil arm: convoking councils, exiling bishops, executing penal statutes against deviants, in exchange for a uniform cult that stabilized taxation, oath-taking, and administration. Their commitment was always revocable — rulers redirected or withdrew enforcement when political calculus changed, and modern states disestablished entirely, removing the arrangement's heaviest instrument.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, secular_rulers, beneficiary,
    powerful, generational, mobile, continental).

% Maintained rival articulations — Arian exegetical networks, Antiochene and Alexandrian factions before definition, later confessional traditions — with scholarly apparatus comparable to the side that won. They were excluded from the conciliar process that fixed the binding text; their objections survive in documents composed outside the enforcement perimeter, and their descendants live as separate communions or as absorbed memories.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, rival_theological_schools, excluded,
    organized, biographical, trapped, regional).

% Reconstruct the controversies, the voting margins, the imperial pressure on councils, and the career consequences of position-taking. They hold no seat in adjudication and attest the genealogy from outside the benefiting parties, including the parts the enforcing communions do not emphasize.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, historians_of_doctrine, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nicene_creed_authority__strict_orthodox_reading, hierarchical_clergy).
narrative_ontology:fixing_cost_class(nicene_creed_authority__strict_orthodox_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single shared metaphysical vocabulary across dispersed congregations: common worship, reliable catechesis, cross-generational transmission of doctrine, and a determinate boundary of communal membership that any congregation anywhere can verify.
% TRANSFER_FUNCTION: Moves interpretive authority from all believers to the clerical hierarchy, and moves sanctions — exclusion from communion, censure, and historically civil penalties up to death — onto those whose metaphysical assent deviates from the defined text.
% ABSENT_VOICES: Heterodox communities and independent lay readers appear only as defendants before tribunals they do not staff; rival theological schools were excluded from the conciliar process that fixed the binding terms. Their objections survive in writings composed outside the enforcement perimeter, which is where they are.
% DISAPPEARANCE_RATIONALE: If binding-metaphysical-assent-enforced-by-sanction vanished overnight, communions would reorganize around voluntary confessional association; the pluralism currently contained by enforcement would surface immediately as competing readings of the same text; the adjudication office would lose its object, and the sanction machinery would have nothing left to administer.
% FOUNDING_PROBLEM: The fourth-century Trinitarian controversies threatened permanent fragmentation of the communion into incompatible theologies, each with scriptural warrant and popular followings; a shared rule of faith was needed to preserve apostolic teaching and communal unity across congregations that had no other coordination mechanism.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the Arian controversy working outside the enforcing communions corroborate that the fragmentation problem was real and acute in the fourth century. Whether it remains live is disputed along seat lines: enforcing communions attest its persistence (doctrinal error, they argue, regenerates whenever vigilance lapses), while ecumenical bodies and mainline scholarship attest that the unifying function is long achieved and what persists is the enforcement apparatus. No seat outside the beneficiary set attests that the sanction mechanism specifically remains necessary.
narrative_ontology:disappearance_verdict(nicene_creed_authority__strict_orthodox_reading, world_rearranges).
narrative_ontology:founding_problem_status(nicene_creed_authority__strict_orthodox_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_creed_authority__strict_orthodox_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nicene_creed_authority__strict_orthodox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nicene_creed_authority__strict_orthodox_reading, 0.64, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_creed_authority__strict_orthodox_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nicene_creed_authority__strict_orthodox_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nicene_creed_authority__strict_orthodox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.64 at interval end, peaking 0.76 at the confessionalization era) because the arrangement transfers interpretive authority wholesale to one seat and prices deviation in communion, community, and historically life and limb — a rate decoupled from any service the payer receives. Suppression (0.55, raw and unscaled — only extractiveness is scaled by directionality and scope in the engine's computation) reflects the residual sanction machinery: excommunication and canonical penalties remain live in enforcing communions even after states withdrew the civil arm. Theater_ratio (0.45) has risen monotonically across the whole interval: anathemas pronounced against long-dead opponents, boundary maintenance performed where no boundary-crosser remains, and recitation treated as assent where assent is unverifiable — the leading indicator that enforcement is becoming performance. Accessibility_collapse (0.60) is partial: the sibling readings, rival communions, and plain secular exit all persist as alternatives, yet within the reading's own frame every alternative is pre-classified as loss of salvation, which collapses perceived alternatives for the identity-locked. Resistance (0.60) has been continuous — Donatist through Reformation through modernist crises — and was overcome historically only by the ruler alliance, not by persuasion alone. The suppression_requirement series is authored deliberately because enforcement-capacity change IS this story's dynamic: a ratchet from conciliar censure (0.35) through inquisitorial maturity (0.80) followed by decay to ecclesial-only sanction (0.55). All three metric series run on one shared grid (t = 0, 250, 500, 750, 1000, 1250, 1500, 1700 years from Nicaea) so every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergent types from identical structural data. From the agenda-setter seat (hierarchical_clergy), the arrangement presents as sacred obligation faithfully administered: the adjudication office cannot experience its own enforcement as extraction because its authority is constituted by the enforcement. From the payer seats, the same structure computes as enforced transfer with suppressed exits — heterodox_communities (trapped) sit nearest the full-target end, lay_interpreters (identity_locked) slightly less far, ordinary_believers near-symmetric. The excluded seat (rival_theological_schools) experiences the arrangement as illegitimate exclusion rather than extraction proper: the injury is being barred from the conversation that defined the binding terms. Coalition dynamics matter for the weakest seat: heterodox communities repeatedly attempted coalition (Donatist Africa, Reformation alliances), and coalition power briefly rivaled the enforcement machinery wherever a ruler defected from the alliance — the arrangement's historical vulnerability was never argument but ruler defection, which is why secular_rulers carries a mobile exit option the other seats lack.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. hierarchical_clergy is the declared beneficiary and agenda-setter with identity_locked exit: its d sits near the beneficiary end (roughly 0.05-0.10) — the arrangement subsidizes it with adjudication authority, and it cannot exit without dissolving its own office. secular_rulers benefits incidentally (uniform cult as administrative stabilizer) but holds arbitrage-grade exit — it withdrew enforcement when calculus changed — placing it nearer the middle-low range despite beneficiary role. ordinary_believers, listed in neither array, derive near-symmetric d (roughly 0.45): genuine coordination benefit (determinate identity, trans-local community) against the assent obligation. lay_interpreters (declared victim, identity_locked) derive high d (roughly 0.85); heterodox_communities (declared victim, trapped) derive the highest d (roughly 0.95), amplified by exitlessness. No directionality overrides are needed: the derivation from declarations plus exit options captures every seat's relationship, including the ruler's defection-prone position, which the arbitrage exit atom already encodes.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — fourth-century Trinitarian fragmentation threatening permanent dissolution of the communion — was real and is corroborated from outside the benefiting parties; whether it remains live is genuinely contested, so the story authors founding_problem_status: contested rather than dead, avoiding a false zombie flag. The tangled_rope classification is what keeps both faces of the arrangement visible: a pure-snare reading would erase the real unifying achievement (the creed did solve a coordination problem no voluntary mechanism solved at the time), while a pure-rope reading would erase the victims (the sanction apparatus fell overwhelmingly on the powerless and the identity-locked). The mandatrophy risk runs forward, not backward: as enforcement capacity decays (suppression_requirement 0.80 to 0.55) while recitation continues, the theater_ratio climbs steadily (0.15 to 0.45). If that crossing completes — enforcement fully symbolic, function carried by the sibling readings instead — this reading degrades toward a piton: anathemas maintained by inertia, administered by an office whose administering is all that remains. The theater series is the leading indicator to watch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading (strict_orthodox_reading) of the nicene_creed_authority kernel; would the sibling readings (symbolic_confessional_reading, liturgical_habituation_reading) instantiate structurally different constraints with different victim sets?',
    'Comparative classification of the sibling stories: if the siblings compute negligible extraction and carry no heretic-victim set, the contest is between a coercive and a non-coercive instantiation of the same fixed text.',
    'If the siblings classify as low-extraction coordination, the strict reading''s extraction is attributable to the reading rather than the creed text itself, relocating reform pressure from the kernel to this reading''s enforcement premises.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the measured extraction belongs to the kernel or to this specific reading of it.').

omega_variable(
    sanction_constitutiveness,
    'Is the sanction mechanism constitutive of the strict reading''s binding claim, or an administrative accretion separable from the requirement of metaphysical assent itself?',
    'Compare communions professing identical Nicene metaphysics with and without enforced penalty for deviation: if binding force survives without sanction in otherwise-equivalent communions, the sanction layer is separable.',
    'If separable, the extractive component is removable policy and the reading could shed it without collapsing into a sibling; if constitutive, relaxing sanction dissolves this reading into the symbolic or liturgical instantiation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sanction_constitutiveness, conceptual, 'Whether sanction is load-bearing for the strict reading or detachable administration.').

omega_variable(
    suppression_internalization,
    'Is the measured suppression structural (tribunals, penalties, exclusion) or internalized (generations formed under sanction come to assent sincerely, carrying the enforcement inward as conscience)?',
    'Post-liberalization trajectory analysis: track whether dissent re-emerges in communions after coercive capacity lapses; rapid surfacing indicates structural suppression, slow or absent surfacing indicates internalization.',
    'If substantially internalized, effective suppression exceeds the structural measure and persists after the 1500-1700 enforcement decay, meaning the scalar suppression value understates the lived constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Structural versus internalized suppression mechanism in doctrinal enforcement.').

omega_variable(
    uniformity_necessity,
    'Is a single metaphysical ontology a requirement of the faith''s coherence (presented by the reading as a structural feature of revealed truth) or a constructed interest of the adjudicating office?',
    'Examine communities sustaining doctrinal depth under the sibling readings: if plural-formal or performative readings show the doctrinal dissolution the strict reading predicts, necessity gains support; if they do not, the naturality claim weakens.',
    'If necessity fails, the reading''s presented-as-natural character collapses and beneficiary-driven construction becomes the parsimonious account; if it holds, part of the measured extraction is the price of the coherence the reading claims to protect.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(uniformity_necessity, conceptual, 'Whether metaphysical uniformity is intrinsic to the tradition or serves the adjudicating seat.').

omega_variable(
    enforcement_revival_pressure,
    'Will the post-1500 decay of coercive sanction capacity in enforcing communions continue, stabilize, or reverse?',
    'Track canonical penalty practice, communion-discipline cases, and church-state legal relations in enforcing communions over coming decades.',
    'Revival would push the arrangement back toward its 1000-1250 high-suppression profile; continued decay combined with rising theater_ratio points toward inertial theatrical maintenance unless the unifying function reasserts independently of coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_revival_pressure, empirical, 'Future trajectory of enforcement capacity in enforcing communions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_creed_authority__strict_orthodox_reading, 0, 1700).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nicene_strict_tr_t0, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(nicene_strict_tr_t0, observed).
narrative_ontology:measurement(nicene_strict_tr_t250, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 250, 0.2).
narrative_ontology:measurement_basis(nicene_strict_tr_t250, observed).
narrative_ontology:measurement(nicene_strict_tr_t500, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 500, 0.24).
narrative_ontology:measurement_basis(nicene_strict_tr_t500, observed).
narrative_ontology:measurement(nicene_strict_tr_t750, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 750, 0.28).
narrative_ontology:measurement_basis(nicene_strict_tr_t750, observed).
narrative_ontology:measurement(nicene_strict_tr_t1000, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 1000, 0.34).
narrative_ontology:measurement_basis(nicene_strict_tr_t1000, observed).
narrative_ontology:measurement(nicene_strict_tr_t1250, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 1250, 0.38).
narrative_ontology:measurement_basis(nicene_strict_tr_t1250, observed).
narrative_ontology:measurement(nicene_strict_tr_t1500, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 1500, 0.42).
narrative_ontology:measurement_basis(nicene_strict_tr_t1500, observed).
narrative_ontology:measurement(nicene_strict_tr_t1700, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 1700, 0.45).
narrative_ontology:measurement_basis(nicene_strict_tr_t1700, observed).

% Extraction over time
narrative_ontology:measurement(nicene_strict_be_t0, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(nicene_strict_be_t0, observed).
narrative_ontology:measurement(nicene_strict_be_t250, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 250, 0.52).
narrative_ontology:measurement_basis(nicene_strict_be_t250, observed).
narrative_ontology:measurement(nicene_strict_be_t500, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 500, 0.58).
narrative_ontology:measurement_basis(nicene_strict_be_t500, observed).
narrative_ontology:measurement(nicene_strict_be_t750, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 750, 0.63).
narrative_ontology:measurement_basis(nicene_strict_be_t750, observed).
narrative_ontology:measurement(nicene_strict_be_t1000, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 1000, 0.72).
narrative_ontology:measurement_basis(nicene_strict_be_t1000, observed).
narrative_ontology:measurement(nicene_strict_be_t1250, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 1250, 0.76).
narrative_ontology:measurement_basis(nicene_strict_be_t1250, observed).
narrative_ontology:measurement(nicene_strict_be_t1500, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 1500, 0.7).
narrative_ontology:measurement_basis(nicene_strict_be_t1500, observed).
narrative_ontology:measurement(nicene_strict_be_t1700, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 1700, 0.64).
narrative_ontology:measurement_basis(nicene_strict_be_t1700, observed).

% Suppression requirement over time
narrative_ontology:measurement(nicene_strict_su_t0, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(nicene_strict_su_t0, observed).
narrative_ontology:measurement(nicene_strict_su_t250, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 250, 0.55).
narrative_ontology:measurement_basis(nicene_strict_su_t250, observed).
narrative_ontology:measurement(nicene_strict_su_t500, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 500, 0.6).
narrative_ontology:measurement_basis(nicene_strict_su_t500, observed).
narrative_ontology:measurement(nicene_strict_su_t750, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 750, 0.65).
narrative_ontology:measurement_basis(nicene_strict_su_t750, observed).
narrative_ontology:measurement(nicene_strict_su_t1000, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 1000, 0.74).
narrative_ontology:measurement_basis(nicene_strict_su_t1000, observed).
narrative_ontology:measurement(nicene_strict_su_t1250, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 1250, 0.8).
narrative_ontology:measurement_basis(nicene_strict_su_t1250, observed).
narrative_ontology:measurement(nicene_strict_su_t1500, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 1500, 0.66).
narrative_ontology:measurement_basis(nicene_strict_su_t1500, observed).
narrative_ontology:measurement(nicene_strict_su_t1700, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 1700, 0.55).
narrative_ontology:measurement_basis(nicene_strict_su_t1700, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_creed_authority__strict_orthodox_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nicene_creed_authority__strict_orthodox_reading, nicene_creed_authority__symbolic_confessional_reading).
narrative_ontology:affects_constraint(nicene_creed_authority__strict_orthodox_reading, nicene_creed_authority__liturgical_habituation_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'authority of the Nicene Creed' decomposes into three structurally distinct constraints sharing one fixed text. This story (strict_orthodox_reading) is the downstream, most extractive member: it cites the text's definitional history as warrant for a binding-assent-plus-sanction regime (epsilon 0.64, tangled_rope). The symbolic_confessional_reading (upstream, lower extraction) treats the same text as contingent witness; the liturgical_habituation_reading treats it as performed identity marker. The upstream readings influence this one by supplying the text and the gathered practice it enforces; this reading influences them by defining their deviations as heresy wherever it governs. Epsilon differs across the family because the readings instantiate different constraints — different compliance conditions, different adjudicators, different victim sets — not because one text is measured inconsistently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
