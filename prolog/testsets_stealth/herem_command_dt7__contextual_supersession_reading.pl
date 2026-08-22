% ============================================================================
% CONSTRAINT STORY: herem_command_dt7__contextual_supersession_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_herem_command_dt7__contextual_supersession_reading, []).

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
 *   constraint_id: herem_command_dt7__contextual_supersession_reading
 *   human_readable: Herem Command (Deut 7) — Contextual Supersession Reading
 *   domain: religious/hermeneutical/ethical
 *
 * SUMMARY:
 *   The herem command of Deuteronomy 7 — the injunction to destroy the
 *   nations of Canaan and refuse intermarriage with them — is a contested
 *   kernel with three live readings. This story instantiates the
 *   contextual_supersession_reading alone: the command was a historically
 *   bounded directive for ancient Israel's settlement period, morally
 *   superseded by the prophetic universalist arc (and, in Christian frames,
 *   by the new covenant). The standing arrangement under contest is the
 *   command's normative operation as this reading instantiates it: the
 *   community retains the canon while declaring the command's violence
 *   delegitimated, relocating membership from ethnic descent to consent and
 *   belief. Assessed by the reading's own lights, the categorical ban is
 *   dissolved and intermarriage is no longer covenant betrayal — but the
 *   arrangement the reading maintains (the canon's continued authority plus
 *   the hermeneutic that neutralizes it) leaves a narrow residual margin
 *   where coercion persists: interfaith couples in communities that never
 *   adopted the neutralization, shunned by enforcement the reading condemns
 *   but cannot reach. Constraint family: this is one of three sibling stories
 *   decomposing the colloquial label 'the herem command' per the
 *   epsilon-invariance principle. The durable_separation_reading instantiates
 *   high extractiveness (a timeless ethnic mandate with a categorical,
 *   trans-generational victim class); the allegorical_displacement_reading
 *   dissolves the ethnic victim class entirely (the command's referent
 *   relocates to inner spiritual enemies, near-zero extraction on
 *   intermarriage); this reading sits between — the command is real but
 *   bounded and superseded, so extractiveness is low with a narrow victim
 *   set. Each is a separate file; all three are linked via
 *   network.affects_constraints. The claimed type and the metrics are
 *   independent authored facts: I claim tangled_rope because the arrangement
 *   holds both a genuine coordination function and asymmetric residual
 *   extraction under active enforcement; the metrics describe what I believe
 *   is descriptively true of its operation.
 *
 * KEY AGENTS:
 *   - liberal_interpretive_institutions: agenda_setter and beneficiary (institutional/constrained) — administers the supersession hermeneutic, collects interpretive authority from it
 *   - interfaith_families: primary beneficiary (moderate/mobile) — freed from the categorical ban, holds unconditioned membership
 *   - interfaith_couples_in_enforcing_communities: primary target (powerless/constrained) — bears residual shunning and exclusion the reading condemns but cannot reach
 *   - residual_enforcing_communities: co-administrator of the residual operation (organized/identity_locked) — enforces endogamy, collects the residual gains, reads the neutralization as betrayal
 *   - dissenting_conservative_scholars: excluded voice (organized/identity_locked) — holds the durable reading, absent from the liberal interpretive conversation
 *   - hermeneutics_observers: analytical observer (analytical/analytical) — sees the full structure across all three readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(herem_command_dt7__contextual_supersession_reading, 0.3).
domain_priors:suppression_score(herem_command_dt7__contextual_supersession_reading, 0.4).
domain_priors:theater_ratio(herem_command_dt7__contextual_supersession_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herem_command_dt7__contextual_supersession_reading, tangled_rope).
narrative_ontology:human_readable(herem_command_dt7__contextual_supersession_reading, "Herem Command (Deut 7) — Contextual Supersession Reading").
narrative_ontology:topic_domain(herem_command_dt7__contextual_supersession_reading, "religious/hermeneutical/ethical").

domain_priors:requires_active_enforcement(herem_command_dt7__contextual_supersession_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__contextual_supersession_reading, 'd5884a2d-ddb1-4809-a6c2-4f09a2cb7ec5').
narrative_ontology:cs_kernel_codification('d5884a2d-ddb1-4809-a6c2-4f09a2cb7ec5', fixed_text).
narrative_ontology:cs_authority_grounding('d5884a2d-ddb1-4809-a6c2-4f09a2cb7ec5', expertise).
narrative_ontology:cs_interpretation_layer_present('d5884a2d-ddb1-4809-a6c2-4f09a2cb7ec5').
narrative_ontology:cs_reading_relation('d5884a2d-ddb1-4809-a6c2-4f09a2cb7ec5', herem_command_dt7__durable_separation_reading, forecloses).
narrative_ontology:cs_reading_relation('d5884a2d-ddb1-4809-a6c2-4f09a2cb7ec5', herem_command_dt7__allegorical_displacement_reading, coexists_with).
narrative_ontology:cs_axiom('d5884a2d-ddb1-4809-a6c2-4f09a2cb7ec5', foundational, herem_binding_force_time_indexed).
narrative_ontology:cs_axiom_status(herem_binding_force_time_indexed, holdable).
narrative_ontology:cs_axiom_grounding('d5884a2d-ddb1-4809-a6c2-4f09a2cb7ec5', herem_binding_force_time_indexed, empirically_contingent).
narrative_ontology:cs_axiom('d5884a2d-ddb1-4809-a6c2-4f09a2cb7ec5', foundational, prophetic_universalism_morally_overrides_herem).
narrative_ontology:cs_axiom_status(prophetic_universalism_morally_overrides_herem, holdable).
narrative_ontology:cs_axiom_grounding('d5884a2d-ddb1-4809-a6c2-4f09a2cb7ec5', prophetic_universalism_morally_overrides_herem, deontological).
narrative_ontology:cs_reference_frame('d5884a2d-ddb1-4809-a6c2-4f09a2cb7ec5', historically_bounded_settlement_directive).
narrative_ontology:cs_drift_state('d5884a2d-ddb1-4809-a6c2-4f09a2cb7ec5', contemporary_literalist_revival_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('d5884a2d-ddb1-4809-a6c2-4f09a2cb7ec5', '').
narrative_ontology:cs_kernel_id(herem_command_dt7__contextual_supersession_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herem_command_dt7__contextual_supersession_reading, interfaith_families).
narrative_ontology:constraint_beneficiary(herem_command_dt7__contextual_supersession_reading, liberal_interpretive_institutions).
narrative_ontology:constraint_victim(herem_command_dt7__contextual_supersession_reading, interfaith_couples_in_enforcing_communities).
narrative_ontology:constraint_vindicates(herem_command_dt7__contextual_supersession_reading, prophetic_universalism).
narrative_ontology:constraint_vindicates(herem_command_dt7__contextual_supersession_reading, historical_critical_method).
narrative_ontology:constraint_vindicates(herem_command_dt7__contextual_supersession_reading, ethical_supersession_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mainline denominations, rabbinical seminaries, and commentary traditions that administer the supersession hermeneutic: they train clergy, set lectionary and curriculum framing, publish the commentaries that bound the command to its historical commission, and discipline hermeneutical deviance within their institutions. They collect institutional authority and interpretive centrality from the framework they administer. Exit would mean surrendering either the canon or the critical method — both constitutive of what they are.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, liberal_interpretive_institutions, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(herem_command_dt7__contextual_supersession_reading, liberal_interpretive_institutions, beneficiary).

% Households formed across the tradition's boundary lines. Under the reading, their marriages are not covenant betrayal and they hold unconditioned membership in the reading's communities. Their exit is comparatively cheap: pluralist and secular milieus are available, and the reading itself removes the bar that once made leaving forced.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, interfaith_families, beneficiary,
    moderate, biographical, mobile, global).

% Couples inside enclaves that never adopted the neutralization. They intermarry against enforced endogamy and bear shunning, disownment, and exclusion from communal rites and burial. The reading's institutions condemn their treatment, but its reach does not extend into the enclaves, and leaving costs natal family ties. Interfaith family networks exist but penetrate enclave boundaries poorly.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, interfaith_couples_in_enforcing_communities, payer,
    powerless, biographical, constrained, global).

% Enclave authorities — rabbinates, elders, congregational boards — that administer enforced endogamy as covenant obligation, drawing warrant from the same canonical text the reading neutralizes elsewhere. They collect boundary cohesion and demographic continuity from the enforcement. Abandoning the practice would dissolve their communal self-understanding, so exit from it is fused with identity; they experience the supersession reading as betrayal rather than as their own arrangement's correction.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, residual_enforcing_communities, agenda_setter,
    organized, generational, identity_locked, global).

% Tradition-holders — conservative theologians, rabbinic authorities, evangelical scholars — who hold the command's force as durable. Their objections circulate in their own venues and are cited by the enclaves, but they are largely absent from the liberal institutions' interpretive conversation. Their scholarly standing is constituted by fidelity to the traditional reading; crossing into the liberal frame would cost them that identity and their home institutions.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, dissenting_conservative_scholars, excluded,
    organized, generational, identity_locked, global).

% Comparative religion scholars and historians of interpretation who trace how the reading was constructed and how its siblings contest it. They hold no stake in the canon's authority and can see the whole structure: the neutralization, the residual field where coercion persists, and the moral question the boundedness claim displaces.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, hermeneutics_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(herem_command_dt7__contextual_supersession_reading, residual_enforcing_communities).
narrative_ontology:fixing_cost_class(herem_command_dt7__contextual_supersession_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the violent-canon problem: how a community keeps a conquest-and-exclusion text in its canon — retaining liturgical continuity, textual scholarship, and identity — without the text authorizing contemporary violence or categorical exclusion. The reading re-coordinates membership: belonging moves from ethnic descent to consent and belief, so the boundary the command once drew is redrawn as voluntary covenant rather than divine mandate against designated peoples.
% TRANSFER_FUNCTION: Moves interpretive authority, legitimacy, and moral burden. Hermeneutical labor flows from clergy and laity to the interpretive institutions (training, commentaries, curricula); full membership and legitimacy flow to interfaith families previously barred by the categorical ban; residual social costs (shunning, disownment) continue to flow from interfaith couples in enclaves that never adopted the neutralization; and the tradition's moral burden for the command's original violence is transferred onto a closed historical past, discharged by the boundedness claim rather than by mourning or reparation.
% ABSENT_VOICES: The annihilated peoples of the original command have no descendant community to speak; the reading's moral accounting proceeds without them, reclassifying their destruction as a bounded historical episode rather than an open moral injury — the loudest absent voice in the arrangement. Conservative tradition-holders are also largely absent from the liberal institutions' interpretive conversation (their objections circulate in separate venues), and enclave members who experience the neutralization as covenant betrayal appear only as objects of the reading's condemnation, never as interlocutors.
% DISAPPEARANCE_RATIONALE: The liberal-institution seats hold that the neutralization is load-bearing: remove the reading and the canon reverts to contested status, literalist application loses its institutional check, and interfaith membership gains come under renewed categorical pressure. Enclave and conservative seats hold the world would be unchanged — they never depended on the reading and regard it as an accommodation to modernity that would simply lapse. The parties genuinely dispute whether arrangements depend on it, so the honest verdict is contested.
% FOUNDING_PROBLEM: The modern reading was built to solve the moral crisis of the canon: a text commanding destruction and categorical exclusion of designated peoples sits in a scripture the community treats as authoritative, and after the Enlightenment — with particular force after the Holocaust — retaining the text while authorizing its violence became untenable. The supersession reading dissociates the community from the command: bind the directive to its historical commission, declare it morally overridden by the tradition's own universalist arc, and keep the canon without the violence.
% FOUNDING_PROBLEM_CORROBORATION: Secular historians of religion and post-Holocaust interfaith dialogue documents — both outside the benefiting parties — attest the founding problem and its continued liveness: the historical-critical scholarship underwriting the reading's method is not produced by its beneficiaries, and post-Shoah theological statements explicitly frame the herem texts as an ongoing moral danger requiring interpretive management. Conservative critics corroborate the problem's existence while disputing the solution: they agree the reading exists to neutralize the text and deny that it should.
narrative_ontology:disappearance_verdict(herem_command_dt7__contextual_supersession_reading, contested).
narrative_ontology:founding_problem_status(herem_command_dt7__contextual_supersession_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(herem_command_dt7__contextual_supersession_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(herem_command_dt7__contextual_supersession_reading, 'none', 1).
narrative_ontology:epsilon_provenance(herem_command_dt7__contextual_supersession_reading, 0.3, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(herem_command_dt7__contextual_supersession_reading_tests).
:- end_tests(herem_command_dt7__contextual_supersession_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The interval anchors to roughly 1935-2025: the consolidation of historical-critical hermeneutics in mainline seminaries through the present. Extractiveness 0.30: the reading dissolves the categorical ethnic ban — under its own lights intermarriage is not covenant betrayal — so the governed population mostly experiences no directive force at all; what remains extractive is the residual margin (enclave coercion of interfaith couples) plus the conformity costs of hermeneutical discipline, low in aggregate because the victim set is narrow but real because the coercion those couples bear is severe. Suppression 0.40: the arrangement must be actively maintained — liberal institutions enforce interpretive orthodoxy against literalist reversion while enclave communities enforce endogamy against their own members; the mechanism is primarily structural (communal enforcement, exclusion from rites and burial) with a secondary internalized component (enclave members who experience endogamy as covenant duty carry the norm after any external barrier falls; roughly 70/30 structural to internalized). Theater_ratio 0.35: the neutralizing function is genuinely operative, but as the founding crisis recedes into institutional memory a growing share of maintenance is ritualized — annual re-explanations of why the command does not bind, hermeneutical disclaimers attached to liturgical readings — and the moral-laundering omega tests whether part of that maintenance is performance rather than reckoning. Accessibility_collapse 0.35: alternatives do not collapse — both sibling readings remain live, exit to pluralist and secular milieus is open, and the reading itself removes the bar that once made exit forced. Resistance 0.55: substantial and organized — conservative repudiation of supersession, scholarly contest over the historical-boundedness claim, enclave rejection of the reading's authority. The suppression_requirement series is authored because the enforcement story genuinely changes over the interval: contested defense of the reading early, routinized maintenance later, offset by hardened enclave enforcement — a gentle net decline. All three series run on one shared grid (seven points, every metric at every point).
 *
 * PERSPECTIVAL GAP:
 *   Three seats compute differently from the same canonical text. From the liberal-institution seat the arrangement is a moral achievement: the text defanged, membership universalized, the tradition kept whole. From the residual-victim seat the same arrangement is an unreachable condemnation — the institutions disavow the coercion done in the text's name while the text's continued canonical authority keeps supplying the warrant the enclaves cite; the victims sit outside the institutions' protective reach precisely because they sit inside the enclaves' kinship webs. From the enclave seat the reading is not a neutralization but a betrayal of covenant, and its institutional carriers are adversaries rather than authorities. The engine computes these per-seat classifications from the structural data; this story's claim adjudicates none of them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. interfaith_families (declared beneficiary, mobile exit) sit near the full-beneficiary end — the reading subsidizes them with unconditioned membership. liberal_interpretive_institutions (declared beneficiary and agenda_setter, constrained exit) sit low-moderate — they collect interpretive authority but bear the maintenance costs of defending the hermeneutic. interfaith_couples_in_enforcing_communities (declared victim, constrained exit) sit near the full-target end — they bear the residual coercion with costly exit. residual_enforcing_communities (agenda_setter of the residual operation, identity_locked) collect the enforcement's gains while bearing the contest's costs, placing them on the beneficiary side of this arrangement's residual extraction. dissenting_conservative_scholars (excluded, identity_locked) bear contest costs without collecting gains. No directionality overrides are authored: the declarations plus exit options already differentiate the seats, and the two organized-power agents (enclaves and scholars) occupy opposed structural positions that a power-atom-keyed override could not separate. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and scope, in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two mislabelings. Labeled rope, the residual victims vanish from the ledger — incomplete reach would be scored as success, and the shunned couples would disappear behind the neutralization's achievements. Labeled snare, the genuine coordination function is erased — the reading does solve the violent-canon problem, relocate membership to consent, and delegitimate the command's violence — and it would be conflated with the durable sibling it exists to oppose. Tangled_rope holds both truths: the same structure that coordinates the tradition's non-violent relationship to the canon extracts, at its unreached margin, from those the neutralization was meant to protect. Mandatrophy: the founding problem — the canon's moral danger — is live (the text remains canonically present and literalist readings circulate), so no mandatrophy resolution is declared; the arrangement has not outlived its function. Identity-lock dynamics bear watching: the enclaves' separation mandate is constitutive of their self-understanding, so if a major enclave authority re-read the command as bounded, the residual enforcement would lose its warrant and the victim set would collapse — the constraint's residual life is held up by fused identity more than by argument. Coalition note: the powerless victim seat could in principle coalize (interfaith family networks exist), but enclave boundaries limit their reach into the coercing communities.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story instantiates only the contextual_supersession_reading of kernel herem_command_dt7; what would each sibling reading change structurally if adopted instead, and where exactly is the disagreement located?',
    'No data resolves a kernel contest; resolution is comparative structural analysis across the three sibling stories — each reading''s own extractiveness, victim set, and enforcement structure, compiled as separate constraint files.',
    'If the durable_separation_reading were adopted, the command''s binding force becomes timeless, the victim class expands to all designated out-marriers across all periods, and extraction rises sharply. If the allegorical_displacement_reading were adopted, the ethnic victim class dissolves entirely (the command''s referent relocates to inner spiritual enemies) and this story''s residual victims lose their textual warrant. The disagreement is located in two structural elements: the command''s temporal index (bounded versus timeless) and the referent of ''the nations'' (ethnic peoples versus typological placeholders).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: one reading of the herem kernel; siblings would change the command''s temporal scope and victim-class definition.').

omega_variable(
    residual_victim_attribution,
    'Do the residual victims — interfaith couples coerced in enforcing communities — belong to this reading''s constraint or to the durable_separation_reading''s instantiation, given that the enclaves administering their coercion explicitly reject the supersession hermeneutic?',
    'Trace the enforcement warrant: if the coercion cites only the durable reading''s doctrine, the victims migrate to the sibling story; if the coercion draws on the canonical text''s continued authority — which this reading maintains while neutralizing it — the victims remain here as this arrangement''s unreached margin.',
    'If attributed wholly to the sibling, this story''s extractiveness falls further (toward rope) and its victim set empties; if inseparable from the shared canon''s authority, the tangled_rope structure holds with the narrow victim set as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_victim_attribution, conceptual, 'Boundary question: which reading''s arrangement owns the residual coercion victims.').

omega_variable(
    supersession_as_moral_laundering,
    'Is the reading''s neutralization a genuine ethical supersession, or does it extract moral legitimacy by reclassifying the command''s annihilated peoples as a closed historical question — discharging the tradition''s moral burden by interpretive disclaimer rather than mourning or reparation?',
    'Survey whether communities holding the reading incorporate reckoning practices — liturgical lament for the command''s victims, curricula teaching the destruction as moral injury rather than fulfilled history — or only interpretive disclaimers that move past the text.',
    'If laundering, the theater_ratio is understated and part of the coordination function is cover; the reading''s extractiveness rises because the beneficiary seats collect moral legitimacy that the original command''s victims never received back. If genuine reckoning is present, the neutralization is substantive and the current metrics stand.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supersession_as_moral_laundering, conceptual, 'Whether supersession neutralizes the command''s morality or launders it.').

omega_variable(
    historical_boundedness_claim,
    'Was herem actually confined to the settlement period — did its practice lapse as the reading claims — or did the ideology persist and revive in later biblical and Second Temple material, weakening the reading''s foundational historical premise?',
    'Diachronic philological analysis of herem''s attestation: frequency, application, and institutional use across periods, assessed independently of the reading''s beneficiary institutions.',
    'If the boundedness claim fails, the reading loses its empirical foundation and weight shifts toward the allegorical sibling (which does not depend on historical bounding) or toward renewed contest; the axiom herem_binding_force_time_indexed is empirically contingent and routes to foreclosure risk if overridden by evidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_boundedness_claim, empirical, 'Empirical foundation: whether herem''s practice actually lapsed after the settlement period.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__contextual_supersession_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(herem_ctx_sup_tr_t0, herem_command_dt7__contextual_supersession_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(herem_ctx_sup_tr_t0, observed).
narrative_ontology:measurement(herem_ctx_sup_tr_t15, herem_command_dt7__contextual_supersession_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement_basis(herem_ctx_sup_tr_t15, observed).
narrative_ontology:measurement(herem_ctx_sup_tr_t30, herem_command_dt7__contextual_supersession_reading, theater_ratio, 30, 0.23).
narrative_ontology:measurement_basis(herem_ctx_sup_tr_t30, observed).
narrative_ontology:measurement(herem_ctx_sup_tr_t45, herem_command_dt7__contextual_supersession_reading, theater_ratio, 45, 0.26).
narrative_ontology:measurement_basis(herem_ctx_sup_tr_t45, observed).
narrative_ontology:measurement(herem_ctx_sup_tr_t60, herem_command_dt7__contextual_supersession_reading, theater_ratio, 60, 0.29).
narrative_ontology:measurement_basis(herem_ctx_sup_tr_t60, observed).
narrative_ontology:measurement(herem_ctx_sup_tr_t75, herem_command_dt7__contextual_supersession_reading, theater_ratio, 75, 0.32).
narrative_ontology:measurement_basis(herem_ctx_sup_tr_t75, observed).
narrative_ontology:measurement(herem_ctx_sup_tr_t90, herem_command_dt7__contextual_supersession_reading, theater_ratio, 90, 0.35).
narrative_ontology:measurement_basis(herem_ctx_sup_tr_t90, observed).

% Extraction over time
narrative_ontology:measurement(herem_ctx_sup_be_t0, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(herem_ctx_sup_be_t0, observed).
narrative_ontology:measurement(herem_ctx_sup_be_t15, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 15, 0.45).
narrative_ontology:measurement_basis(herem_ctx_sup_be_t15, observed).
narrative_ontology:measurement(herem_ctx_sup_be_t30, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement_basis(herem_ctx_sup_be_t30, observed).
narrative_ontology:measurement(herem_ctx_sup_be_t45, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 45, 0.38).
narrative_ontology:measurement_basis(herem_ctx_sup_be_t45, observed).
narrative_ontology:measurement(herem_ctx_sup_be_t60, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 60, 0.35).
narrative_ontology:measurement_basis(herem_ctx_sup_be_t60, observed).
narrative_ontology:measurement(herem_ctx_sup_be_t75, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 75, 0.32).
narrative_ontology:measurement_basis(herem_ctx_sup_be_t75, observed).
narrative_ontology:measurement(herem_ctx_sup_be_t90, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 90, 0.3).
narrative_ontology:measurement_basis(herem_ctx_sup_be_t90, observed).

% Suppression requirement over time
narrative_ontology:measurement(herem_ctx_sup_su_t0, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(herem_ctx_sup_su_t0, observed).
narrative_ontology:measurement(herem_ctx_sup_su_t15, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 15, 0.49).
narrative_ontology:measurement_basis(herem_ctx_sup_su_t15, observed).
narrative_ontology:measurement(herem_ctx_sup_su_t30, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 30, 0.47).
narrative_ontology:measurement_basis(herem_ctx_sup_su_t30, observed).
narrative_ontology:measurement(herem_ctx_sup_su_t45, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 45, 0.46).
narrative_ontology:measurement_basis(herem_ctx_sup_su_t45, observed).
narrative_ontology:measurement(herem_ctx_sup_su_t60, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 60, 0.44).
narrative_ontology:measurement_basis(herem_ctx_sup_su_t60, observed).
narrative_ontology:measurement(herem_ctx_sup_su_t75, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 75, 0.42).
narrative_ontology:measurement_basis(herem_ctx_sup_su_t75, observed).
narrative_ontology:measurement(herem_ctx_sup_su_t90, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 90, 0.4).
narrative_ontology:measurement_basis(herem_ctx_sup_su_t90, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(herem_command_dt7__contextual_supersession_reading, identity_coordination).
narrative_ontology:affects_constraint(herem_command_dt7__contextual_supersession_reading, herem_command_dt7__durable_separation_reading).
narrative_ontology:affects_constraint(herem_command_dt7__contextual_supersession_reading, herem_command_dt7__allegorical_displacement_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the herem command' covers three structurally distinct claims that cannot share one epsilon: (1) a timeless binding mandate (durable_separation_reading — high extractiveness, categorical trans-generational victim class); (2) a historically bounded directive now morally superseded (this story — low extractiveness, narrow residual victim set); (3) a typological allegory of inner moral warfare (allegorical_displacement_reading — ethnic victim class dissolved). Measuring the constraint one way yields clearly low extractiveness and another way clearly high, so per the epsilon-invariance principle these are three constraints, not one. This file links both siblings via network.affects_constraints; the siblings document the same decomposition from their own seats. The upstream/downstream structure runs from the durable reading (the tradition's inherited default) through this reading (the critical correction) to the allegorical reading (the devotional displacement).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
