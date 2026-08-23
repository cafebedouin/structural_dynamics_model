% ============================================================================
% CONSTRAINT STORY: jihad_quranic_corpus__defensive_spiritual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jihad_quranic_corpus__defensive_spiritual_reading, []).

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
 *   constraint_id: jihad_quranic_corpus__defensive_spiritual_reading
 *   human_readable: Defensive-Spiritual Jihad Framework: Jihad al-Nafs Primacy with Authority-Gated Defensive Force
 *   domain: religious law/political theology/comparative religious law
 *
 * SUMMARY:
 *   This story instantiates ONE reading of a contested kernel. The kernel is
 *   the Qur'anic-prophetic corpus on jihad; this file carries the
 *   defensive-spiritual reading: the primary jihad is the internal struggle
 *   (jihad al-nafs), and armed force is legitimate only as defense against
 *   aggression, declared by legitimate authority, bound by proportionality
 *   and non-combatant immunity, with a coexistence framework privileged
 *   toward non-Muslim neighbors. The expansionist-legalist reading (offensive
 *   campaigns permissible under jurisprudential conditions) and the
 *   revolutionary-vanguard reading (ungated individual obligation via takfir)
 *   are separate constraints — separate files with their own epsilon, victim
 *   sets, and enforcement structures — linked through the network block; the
 *   contest between readings is routed to omega variables, not averaged into
 *   this one. Within this reading's own lights, the operative arrangement is
 *   a violence-legitimation framework that solves a genuine collective-action
 *   problem (unregulated confessional violence, fitna) while accruing a
 *   modest authority rent to the gatekeeping axis and concentrating real
 *   costs on believers whose defensive gate does not open. KEY AGENTS (by
 *   structural relationship): - legitimate_state_authorities: agenda-setter
 *   (institutional/arbitrage) — holds the legitimation monopoly, enforces the
 *   gate - ulama_establishments: beneficiary and co-administrator
 *   (institutional/identity_locked) — interpretive gatekeeping; accrues the
 *   reading's characteristic rent - muslim_civilian_populations and
 *   non_muslim_neighbor_communities: beneficiaries (moderate/constrained) —
 *   protected by the defensive trigger and non-combatant immunity -
 *   ordinary_believers: beneficiary with payer secondary
 *   (moderate/identity_locked) — receive the al-nafs channel, bear the
 *   foreclosed-agency discipline - believers_under_occupation: primary target
 *   (powerless/trapped) — bear the concentrated cost of the high threshold -
 *   revolutionary_vanguard_networks: excluded (organized/trapped) — the
 *   suppressed rival claim on the kernel - comparative_jurists: analytical
 *   observer — maps the reading structure without collecting or paying
 *
 * KEY AGENTS:
 *   - legitimate_state_authorities: agenda-setter (institutional/arbitrage) — legitimation monopoly, enforces the gate
 *   - ulama_establishments: beneficiary and co-administrator (institutional/identity_locked) — interpretive gatekeepers, accrue the reading's rent
 *   - muslim_civilian_populations: beneficiary (moderate/constrained) — protected by non-combatant immunity
 *   - non_muslim_neighbor_communities: beneficiary (moderate/constrained) — protected coexistents, outside the victim set unless aggressors
 *   - ordinary_believers: beneficiary with payer secondary (moderate/identity_locked) — receive the al-nafs channel, bear foreclosed agency
 *   - believers_under_occupation: payer (powerless/trapped) — bear the concentrated cost of the high threshold
 *   - revolutionary_vanguard_networks: excluded (organized/trapped) — the suppressed rival claim
 *   - comparative_jurists: observer (analytical/analytical) — map the reading structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jihad_quranic_corpus__defensive_spiritual_reading, 0.25).
domain_priors:suppression_score(jihad_quranic_corpus__defensive_spiritual_reading, 0.5).
domain_priors:theater_ratio(jihad_quranic_corpus__defensive_spiritual_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__defensive_spiritual_reading, rope).
narrative_ontology:human_readable(jihad_quranic_corpus__defensive_spiritual_reading, "Defensive-Spiritual Jihad Framework: Jihad al-Nafs Primacy with Authority-Gated Defensive Force").
narrative_ontology:topic_domain(jihad_quranic_corpus__defensive_spiritual_reading, "religious law/political theology/comparative religious law").

domain_priors:requires_active_enforcement(jihad_quranic_corpus__defensive_spiritual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__defensive_spiritual_reading, '49150543-469e-410a-a8bb-61a00c2ae1c9').
narrative_ontology:cs_kernel_codification('49150543-469e-410a-a8bb-61a00c2ae1c9', fixed_text).
narrative_ontology:cs_authority_grounding('49150543-469e-410a-a8bb-61a00c2ae1c9', lineage).
narrative_ontology:cs_interpretation_layer_present('49150543-469e-410a-a8bb-61a00c2ae1c9').
narrative_ontology:cs_reading_relation('49150543-469e-410a-a8bb-61a00c2ae1c9', jihad_quranic_corpus__expansionist_legalist_reading, forecloses).
narrative_ontology:cs_reading_relation('49150543-469e-410a-a8bb-61a00c2ae1c9', jihad_quranic_corpus__revolutionary_vanguard_reading, forecloses).
narrative_ontology:cs_axiom('49150543-469e-410a-a8bb-61a00c2ae1c9', foundational, armed_force_defensive_only).
narrative_ontology:cs_axiom_status(armed_force_defensive_only, holdable).
narrative_ontology:cs_axiom_grounding('49150543-469e-410a-a8bb-61a00c2ae1c9', armed_force_defensive_only, theological).
narrative_ontology:cs_axiom('49150543-469e-410a-a8bb-61a00c2ae1c9', foundational, legitimate_authority_requirement).
narrative_ontology:cs_axiom_status(legitimate_authority_requirement, holdable).
narrative_ontology:cs_axiom_grounding('49150543-469e-410a-a8bb-61a00c2ae1c9', legitimate_authority_requirement, conventional).
narrative_ontology:cs_reference_frame('49150543-469e-410a-a8bb-61a00c2ae1c9', medinan_defensive_coexistence_norm).
narrative_ontology:cs_drift_state('49150543-469e-410a-a8bb-61a00c2ae1c9', contemporary, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('49150543-469e-410a-a8bb-61a00c2ae1c9', '').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__defensive_spiritual_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, legitimate_state_authorities).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, ulama_establishments).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, muslim_civilian_populations).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, non_muslim_neighbor_communities).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, ordinary_believers).
narrative_ontology:constraint_victim(jihad_quranic_corpus__defensive_spiritual_reading, believers_under_occupation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jihad_quranic_corpus__defensive_spiritual_reading, ordinary_believers).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__defensive_spiritual_reading, non_combatant_immunity_doctrine).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__defensive_spiritual_reading, proportionality_principle).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__defensive_spiritual_reading, fitna_prevention_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claim and administer the authority to declare or withhold legitimate armed jihad; prosecute unilateral armed actors as criminals; invoke the defensive framework to legitimize their own wars and to delegitimize rivals' armed claims. They hold the legitimation monopoly: within their territory, no religiously-motivated force is legitimate except through them. Their position is arbitrage-grade — they can reframe any conflict as defense and any rival's force as rebellion.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, legitimate_state_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Scholarly institutions (al-Azhar, national fatwa councils, transnational juristic networks) interpret the corpus: what counts as aggression, who is a legitimate authority, what proportionality requires. Their interpretive gatekeeping position is constituted by the framework — the reading cannot operate without their arbitration, and their authority cannot exist without the framework. Their scholarly identity is fused with the interpretive office; exit would dissolve the institution itself. The framework's extraction takes its characteristic form in their hands: interpretive authority.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, ulama_establishments, beneficiary,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(jihad_quranic_corpus__defensive_spiritual_reading, ulama_establishments, agenda_setter).

% Receive the framework's protection: non-combatant immunity shields them from being conscripted into or targeted by unregulated religious violence, and the authority gate keeps armed action out of their streets and mosques. They bear indirect costs when their states invoke defense for wars they did not choose, but their net position under the framework is protected.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, muslim_civilian_populations, beneficiary,
    moderate, biographical, constrained, national).

% Live under the coexistence framework this reading privileges: the defensive trigger and non-combatant immunity place them outside the permissible target set unless they are aggressors. Their structural position under this reading is protected coexistence — distinct from their position under the sibling readings, where they would be governed subjects, campaign objectives, or contested populations. Their protection is durable only as long as the institutions holding this reading remain in control.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, non_muslim_neighbor_communities, beneficiary,
    moderate, biographical, constrained, regional).

% Receive the primary channel: the greater jihad of self-discipline (jihad al-nafs) satisfies the struggle obligation without blood-guilt, and armed action is legitimized only through proper authority. They pay the discipline's cost: foreclosed from unilateral action even when they judge aggression to be occurring, and bound to the internal struggle as a standing religious duty. The obligation structure is constitutive of their religious identity — exit from the constraint would mean exit from the faith.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, ordinary_believers, beneficiary,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(jihad_quranic_corpus__defensive_spiritual_reading, ordinary_believers, payer).

% Believers in territories under foreign occupation or aggression. The framework's high threshold — legitimate authority, declared defense — is a gate their situation often cannot open: the occupying power controls the territory, and their own state is absent, complicit, or defeated. The jurisprudence's defensive exception (individual obligation under invasion) is their designed relief valve, but operationally it is slow, contested, and dependent on the same scholarly institutions that counsel patience. They bear the concentrated cost of the framework's order and cannot route around the gate.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, believers_under_occupation, payer,
    powerless, biographical, trapped, regional).

% Non-state armed networks claiming the corpus's authority for ungated violence against apostate rulers and occupiers. The framework excludes their claim: no legitimate authority, no declared defense, takfir rejected. They would argue the authority gate abandons the oppressed to their oppressors and that the establishments' reading serves states rather than God; the framework answers that their method produces fitna. States prosecute them and establishments issue counter-fatwas; their only exit from the framework's reach is its defeat.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, revolutionary_vanguard_networks, excluded,
    organized, generational, trapped, global).

% Academic scholars of Islamic law and comparative religious law who map the reading structure: which readings control which institutions, how the victim sets differ across readings, where the enforcement runs. They neither collect from the framework nor pay its costs; they document its operation and its contest.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, comparative_jurists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jihad_quranic_corpus__defensive_spiritual_reading, ulama_establishments).
narrative_ontology:fixing_cost_class(jihad_quranic_corpus__defensive_spiritual_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Regulates religiously-motivated force: gates armed action behind legitimate authority and a defensive trigger, binds it to proportionality and non-combatant immunity, and designates the internal spiritual struggle as the primary jihad so the struggle obligation does not default to armed action. This solves the collective-action problem of unregulated confessional violence and underwrites coexistence with non-Muslim neighbors.
% TRANSFER_FUNCTION: Moves the authority to legitimize armed force from individuals to the state-and-establishment axis; moves the struggle obligation's default performance from the battlefield to the self; moves security to all civilians as protected status, at the cost of deferred or denied self-help for believers under aggression whose gate does not open.
% ABSENT_VOICES: Revolutionary vanguard networks are excluded — they would argue the authority gate abandons the occupied to their oppressors and that the establishments' reading serves states, not God. Believers under occupation whose relief valve never opens are partially present through sympathetic jurists but rarely seated in the councils that set the thresholds. Both sit outside the official interpretive institutions: in prisons, underground networks, and diaspora critique.
% DISAPPEARANCE_RATIONALE: If the framework vanished overnight, the question of who may legitimize religiously-motivated force reopens with no settled answer: vanguard claims would face no established counter-framework, coexistence norms with non-Muslim neighbors would lose their doctrinal anchor, and every state's defense-of-the-faith claim would compete with every preacher's. The religious arrangements of over a billion believers and the violence-legitimation practices of dozens of states are organized around this reading's answers.
% FOUNDING_PROBLEM: Regulating the believing community's recourse to force: preventing fitna (internecine chaos), unregulated bloodshed, and wars of ambition carried out in the faith's name — and, in the modern era, translating a martial religious vocabulary into a world of nation-states and permanent non-Muslim neighbors.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: comparative international-law scholars and humanitarian-law practitioners document the framework's parallel function to just-war regulation and international humanitarian law; historians of Islamic law outside the establishments, including non-Muslim scholars, attest the fitna-prevention problem's centrality to the siyar tradition; and the vanguard networks' own polemics corroborate it backhandedly — they attack the framework precisely for gating force, confirming that gating is what it does.
narrative_ontology:disappearance_verdict(jihad_quranic_corpus__defensive_spiritual_reading, world_rearranges).
narrative_ontology:founding_problem_status(jihad_quranic_corpus__defensive_spiritual_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jihad_quranic_corpus__defensive_spiritual_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jihad_quranic_corpus__defensive_spiritual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jihad_quranic_corpus__defensive_spiritual_reading, 0.25, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jihad_quranic_corpus__defensive_spiritual_reading_tests).
:- end_tests(jihad_quranic_corpus__defensive_spiritual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. The claim is rope: the framework's dominant work is coordination — gating religiously-motivated force behind authority, a defensive trigger, proportionality, and non-combatant immunity, which protects civilians of all confessions and underwrites coexistence. The metrics describe its actual operation. Extractiveness 0.25: modest but real — the gatekeeping axis accrues interpretive and legitimation rent, and the high threshold concentrates denied self-help on occupied believers. Suppression 0.50: the framework is actively enforced (state prosecution of unilateral armed actors, establishment counter-fatwa campaigns), but the rival readings remain live and intelligible, so alternatives are suppressed in application rather than erased. Theater ratio 0.20: the function is real and load-bearing; the theatrical share is performative condemnation coexisting with selective tolerance, and defensive rhetoric invoked for non-defensive state wars. Accessibility collapse 0.35: a believer who fully understands the authority gate still has the vanguard reading available as an intelligible alternative — it does not collapse. Resistance 0.55: sustained vanguard resistance, sometimes armed, plus juristic dissent from advocates of the occupied. Suppression here is authored as a raw structural property — it is not scaled by power or scope; only extractiveness is scaled downstream by directionality and scope. The measurement series share one grid (points 0–60 at intervals of 10) and trace an enforcement ratchet rather than a cycle: suppression_requirement rises as the machinery against unilateral violence matured across the modern era, base_extractiveness creeps upward as states extend the defensive frame to delegitimize all rival armed claims, and theater rises modestly with the widening gap between official condemnation and selective enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the state seat the arrangement is a legitimate monopoly it built and administers: force is legitimate when it says so, and rivals' force is crime. From the ulama seat it is a juristic achievement: the gate is interpretation, and interpretation is their office. From ordinary believers it is a working channel: the struggle obligation is satisfiable without blood-guilt. From believers under occupation the same gate is a wall: the authority their defense requires does not exist or is complicit, and the relief valve is slow and discretionary. From the excluded vanguard networks it is a usurpation: the establishments' reading serves states, not the oppressed. The engine computes these per-seat classifications from power, exit, and directionality; the authored rope claim does not adjudicate them — and given the declared enforcement plus a declared victim seat, some seats may compute toward the hybrid coordination/extraction type, which is data, not error.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries derive low directionality: muslim_civilian_populations and non_muslim_neighbor_communities (protected status, constrained exit), ordinary_believers (the al-nafs channel; identity_locked exit damps mobility but their net position is subsidized — they receive a satisfiable obligation), legitimate_state_authorities (arbitrage-grade exit: they can reframe any conflict as defense), and ulama_establishments (identity_locked beneficiaries whose position is constituted by the framework). The declared victim seat, believers_under_occupation, derives high directionality: trapped exit amplifies their effective extraction — they cannot route around the gate, and the framework's own defensive exception is their only designed relief. The identity_locked atoms on ordinary_believers and ulama_establishments are religious-identity fusion: the obligation structure is constitutive of the faith, so exit from the constraint would mean exit from the tradition; for the ulama it is professional-institutional fusion — the interpretive office IS the framework's operation. If the identity frame broke (a mass juristic movement declaring the gate a human construction), the enforcement burden would rise sharply and the authored suppression would understate the coercion required.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — regulating the believing community's recourse to force — is live, so no mandatrophy is declared: violence regulation does not retire. The classification work runs in the other direction, against two misreadings. Reading the framework as pure coordination with no cost-bearers would erase the concentrated cost on occupied believers and the gatekeepers' rent. Reading it as the vanguard polemic does — cover for establishment and state power, i.e., pure extraction — would erase the genuine coordination function that protects civilians of all confessions and underwrites coexistence. The rope claim with a declared victim seat and active enforcement keeps both faces in the data. The drift risk to watch is the relief valve: if the defensive exception proves inoperative (see omega defensive_exception_operativity), the cost asymmetry sharpens — the gatekeepers could lower the threshold but do not, while the powerless pay — and the arrangement drifts toward the hybrid type without any change in doctrine.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Which reading of the jihad_quranic_corpus kernel is structurally operative in a given jurisdiction, and what would each sibling reading change if it gained institutional control?',
    'Track which readings'' institutions control fatwa authority, courts, and armed-force legitimation in each jurisdiction; the operative reading is the one whose gating structure actually binds force on the ground.',
    'If the expansionist reading gains control, non-Muslim polities enter the cost-bearing set and extraction rises sharply; if the vanguard reading gains, the authority gate collapses and the coordination function fails. This story''s epsilon and victim set hold only while the defensive-spiritual reading''s institutions bind force.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Kernel-level contest over which reading of the jihad corpus is operative.').

omega_variable(
    textual_warrant_ambiguity,
    'Is the defensive-only reading textually compelled by the corpus, or a modern apologetic reconstruction projecting present coexistence needs onto the text?',
    'Philological and historical analysis of the abrogation debates, the siyar literature, and the distribution of classical juristic positions on offensive versus defensive war; compare this reading''s proof-texts against the classical mainstream''s use of the same corpus.',
    'If the reading is a modern reconstruction, its classification rests on present function rather than textual warrant, and its authority gate becomes more contestable from within the tradition itself, raising the probability of sibling-reading revival.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_warrant_ambiguity, empirical, 'Whether the defensive reading is textually compelled or a modernist reconstruction.').

omega_variable(
    suppression_mechanism_split,
    'Is the suppression of unilateral armed action structural (state prosecution, institutional counter-fatwa machinery) or internalized (believers'' self-discipline under the jihad al-nafs primacy)?',
    'Post-state-collapse trajectory: in jurisdictions where state authority fails, does unilateral armed action surge (suppression was structural) or do communities and scholars continue gating it (suppression is substantially internalized)?',
    'If internalized, the framework survives state collapse and its suppression metric understates its durability; if structural, the framework is state-dependent and the vanguard reading''s opportunity set expands with every state failure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_split, empirical, 'Structural versus internalized suppression of unilateral religious violence.').

omega_variable(
    defensive_exception_operativity,
    'Does the jurisprudence''s defensive exception (armed defense as individual obligation under invasion) actually operate for believers under occupation, or does the authority requirement leave them without a working channel?',
    'Comparative case analysis of establishment fatwa practice across occupied territories: when the exception is invoked, what it authorizes, and how quickly it responds relative to the aggression it answers.',
    'If the relief valve fails operationally, the cost concentrated on believers_under_occupation exceeds the framework''s own accounting, extraction concentrates on the powerless seat, and the reading''s effective epsilon is higher than authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(defensive_exception_operativity, empirical, 'Whether the defensive relief valve works for the occupied.').

omega_variable(
    non_muslim_protection_durability,
    'Does the coexistence framework''s protection of non-Muslim neighbors hold durably, or only while the Muslim polity is secure and confident?',
    'Historical and contemporary analysis of non-Muslim minority treatment under institutions controlled by this reading, across periods of polity strength and weakness.',
    'If protection degrades under stress, non-Muslim communities are latent members of the cost-bearing set and this reading''s epsilon understates its structural risk; the victim-set difference from the expansionist sibling would then be narrower than the reading claims.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(non_muslim_protection_durability, empirical, 'Durability of non-Muslim neighbor protection under polity stress.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__defensive_spiritual_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jqc_def_spiritual_tr_t0, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(jqc_def_spiritual_tr_t0, observed).
narrative_ontology:measurement(jqc_def_spiritual_tr_t10, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 10, 0.11).
narrative_ontology:measurement_basis(jqc_def_spiritual_tr_t10, observed).
narrative_ontology:measurement(jqc_def_spiritual_tr_t20, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement_basis(jqc_def_spiritual_tr_t20, observed).
narrative_ontology:measurement(jqc_def_spiritual_tr_t30, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 30, 0.14).
narrative_ontology:measurement_basis(jqc_def_spiritual_tr_t30, observed).
narrative_ontology:measurement(jqc_def_spiritual_tr_t40, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 40, 0.16).
narrative_ontology:measurement_basis(jqc_def_spiritual_tr_t40, observed).
narrative_ontology:measurement(jqc_def_spiritual_tr_t50, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 50, 0.18).
narrative_ontology:measurement_basis(jqc_def_spiritual_tr_t50, observed).
narrative_ontology:measurement(jqc_def_spiritual_tr_t60, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 60, 0.2).
narrative_ontology:measurement_basis(jqc_def_spiritual_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(jqc_def_spiritual_be_t0, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 0, 0.16).
narrative_ontology:measurement_basis(jqc_def_spiritual_be_t0, observed).
narrative_ontology:measurement(jqc_def_spiritual_be_t10, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 10, 0.17).
narrative_ontology:measurement_basis(jqc_def_spiritual_be_t10, observed).
narrative_ontology:measurement(jqc_def_spiritual_be_t20, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 20, 0.19).
narrative_ontology:measurement_basis(jqc_def_spiritual_be_t20, observed).
narrative_ontology:measurement(jqc_def_spiritual_be_t30, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 30, 0.21).
narrative_ontology:measurement_basis(jqc_def_spiritual_be_t30, observed).
narrative_ontology:measurement(jqc_def_spiritual_be_t40, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 40, 0.22).
narrative_ontology:measurement_basis(jqc_def_spiritual_be_t40, observed).
narrative_ontology:measurement(jqc_def_spiritual_be_t50, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 50, 0.24).
narrative_ontology:measurement_basis(jqc_def_spiritual_be_t50, observed).
narrative_ontology:measurement(jqc_def_spiritual_be_t60, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 60, 0.25).
narrative_ontology:measurement_basis(jqc_def_spiritual_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(jqc_def_spiritual_su_t0, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(jqc_def_spiritual_su_t0, observed).
narrative_ontology:measurement(jqc_def_spiritual_su_t10, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 10, 0.34).
narrative_ontology:measurement_basis(jqc_def_spiritual_su_t10, observed).
narrative_ontology:measurement(jqc_def_spiritual_su_t20, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement_basis(jqc_def_spiritual_su_t20, observed).
narrative_ontology:measurement(jqc_def_spiritual_su_t30, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement_basis(jqc_def_spiritual_su_t30, observed).
narrative_ontology:measurement(jqc_def_spiritual_su_t40, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 40, 0.46).
narrative_ontology:measurement_basis(jqc_def_spiritual_su_t40, observed).
narrative_ontology:measurement(jqc_def_spiritual_su_t50, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 50, 0.48).
narrative_ontology:measurement_basis(jqc_def_spiritual_su_t50, observed).
narrative_ontology:measurement(jqc_def_spiritual_su_t60, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 60, 0.5).
narrative_ontology:measurement_basis(jqc_def_spiritual_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jihad_quranic_corpus__defensive_spiritual_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jihad_quranic_corpus__defensive_spiritual_reading, jihad_quranic_corpus__expansionist_legalist_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__defensive_spiritual_reading, jihad_quranic_corpus__revolutionary_vanguard_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'jihad' covers at least three structurally distinct normative frameworks that the epsilon-invariance principle requires be authored separately — they assign different legitimacy to the same act classes, name different victim sets, and rest on different authority structures, so no single epsilon can describe the label. This file is the defensive-spiritual reading (low epsilon, non-Muslims outside the victim set, state authority required). The upstream/downstream structure runs through interpretive control: whichever reading's institutions control fatwa authority and enforcement binds the corpus in a given jurisdiction, and each reading's persistence changes the others' enforcement burden. Sibling files link back to this constraint_id.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
