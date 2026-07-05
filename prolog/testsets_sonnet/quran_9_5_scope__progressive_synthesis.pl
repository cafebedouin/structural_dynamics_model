% ============================================================================
% CONSTRAINT STORY: quran_9_5_scope__progressive_synthesis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Progressive-Synthesis Reading of Verse 9:5 (Time-Bound Directive, Superseded by Ethical Trajectory)
 *   domain: Islamic Jurisprudence / Hermeneutics / Political Theology
 *
 * SUMMARY:
 *   This story instantiates the progressive_synthesis reading of the
 *   contested kernel governing verse 9:5's scope (kernel_id:
 *   quran_9_5_scope). Under this reading, the verse is a time-bound political
 *   directive addressed to a specific 7th-century Medinan situation whose
 *   legal force has been superseded by the Quran's broader ethical trajectory
 *   toward justice and non-compulsion; the verse exits active constraint
 *   space entirely with respect to both historical polytheist communities and
 *   present-day Muslims. This is a distinct constraint from the sibling
 *   readings abrogating_universal (verse as standing universal legal
 *   obligation) and contextual_defensive (verse as narrowly scoped but not
 *   superseded, still legally live for treaty-breach/defensive contexts) —
 *   each sibling has its own ε, its own beneficiary/victim structure, and is
 *   authored as a separate story, linked here only through network and
 *   cs_structure fields, per the ε-invariance principle.
 *
 * KEY AGENTS:
 *   - reformist_muslim_scholars: agenda_setter/beneficiary (organized/mobile) — advances and institutionalizes the reading
 *   - textualist_authority_structures: payer (institutional/identity_locked) — legitimacy eroded by the reading's spread
 *   - secular_pluralist_frameworks: beneficiary (institutional/analytical) — gains an internal-to-Islam accommodation argument
 *   - lay_muslim_practitioners: beneficiary/excluded (moderate/constrained) — inherits the reading without adjudicating it
 *   - polytheist_and_pagan_communities: beneficiary (powerless/analytical) — the literal referent class exits active constraint scope
 *   - islamist_political_movements: excluded (organized/trapped) — rejects this reading's premises entirely
 *   - comparative_religion_scholars: observer (analytical) — studies the contest without a stake in outcome
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_9_5_scope__progressive_synthesis, 0.42).
domain_priors:suppression_score(quran_9_5_scope__progressive_synthesis, 0.55).
domain_priors:theater_ratio(quran_9_5_scope__progressive_synthesis, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, extractiveness, 0.42).
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_9_5_scope__progressive_synthesis, piton).
narrative_ontology:human_readable(quran_9_5_scope__progressive_synthesis, "Progressive-Synthesis Reading of Verse 9:5 (Time-Bound Directive, Superseded by Ethical Trajectory)").
narrative_ontology:topic_domain(quran_9_5_scope__progressive_synthesis, "Islamic Jurisprudence / Hermeneutics / Political Theology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_9_5_scope__progressive_synthesis, 'dbc37305-de2e-4b0a-b94e-19fe20a82075').
narrative_ontology:cs_kernel_codification('dbc37305-de2e-4b0a-b94e-19fe20a82075', fixed_text).
narrative_ontology:cs_authority_grounding('dbc37305-de2e-4b0a-b94e-19fe20a82075', practice).
narrative_ontology:cs_interpretation_layer_present('dbc37305-de2e-4b0a-b94e-19fe20a82075').
narrative_ontology:cs_reading_relation('dbc37305-de2e-4b0a-b94e-19fe20a82075', quran_9_5_scope__abrogating_universal, forecloses).
narrative_ontology:cs_reading_relation('dbc37305-de2e-4b0a-b94e-19fe20a82075', quran_9_5_scope__contextual_defensive, influences).
narrative_ontology:cs_axiom('dbc37305-de2e-4b0a-b94e-19fe20a82075', foundational, ethical_trajectory_supersedes_literal_verse).
narrative_ontology:cs_axiom_status(ethical_trajectory_supersedes_literal_verse, holdable).
narrative_ontology:cs_axiom_grounding('dbc37305-de2e-4b0a-b94e-19fe20a82075', ethical_trajectory_supersedes_literal_verse, conventional).
narrative_ontology:cs_axiom('dbc37305-de2e-4b0a-b94e-19fe20a82075', foundational, revelation_is_time_indexed_political_speech_act).
narrative_ontology:cs_axiom_status(revelation_is_time_indexed_political_speech_act, holdable).
narrative_ontology:cs_axiom_grounding('dbc37305-de2e-4b0a-b94e-19fe20a82075', revelation_is_time_indexed_political_speech_act, empirically_contingent).
narrative_ontology:cs_reference_frame('dbc37305-de2e-4b0a-b94e-19fe20a82075', classical_naskh_transmission_framework).
narrative_ontology:cs_drift_state('dbc37305-de2e-4b0a-b94e-19fe20a82075', post_20th_century_political_islam_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('dbc37305-de2e-4b0a-b94e-19fe20a82075', '').
narrative_ontology:cs_kernel_id(quran_9_5_scope__progressive_synthesis, quran_9_5_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_9_5_scope__progressive_synthesis, secular_pluralist_frameworks).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__progressive_synthesis, reformist_muslim_scholars).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__progressive_synthesis, interfaith_coexistence_advocates).
narrative_ontology:constraint_victim(quran_9_5_scope__progressive_synthesis, textualist_authority_structures).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__progressive_synthesis, lay_muslim_practitioners).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__progressive_synthesis, polytheist_and_pagan_communities_historical_and_contemporary).
narrative_ontology:constraint_vindicates(quran_9_5_scope__progressive_synthesis, quranic_ethical_trajectory_doctrine).
narrative_ontology:constraint_vindicates(quran_9_5_scope__progressive_synthesis, historical_contextualization_of_revelation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advance a hermeneutic in which verse 9:5 is read as addressed to a specific 7th-century treaty-breach situation and understood to have been legislatively superseded by the Quran's broader ethical arc (justice, pluralism, non-compulsion in religion). They publish, teach, and issue rulings that treat the verse as historically closed rather than operative law, gaining institutional standing in academic and interfaith settings by doing so.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, reformist_muslim_scholars, agenda_setter,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(quran_9_5_scope__progressive_synthesis, reformist_muslim_scholars, beneficiary).

% Seminaries, fatwa councils, and movements whose legal and political legitimacy rests on treating classical tafsir and abrogation doctrine (naskh) as binding continue to assert the verse's standing force. The progressive-synthesis reading, if it gains institutional traction, erodes their interpretive monopoly and the authority claims built on uninterrupted textual continuity; their exit from this dispute is blocked by identity fusion between their institutional legitimacy and the doctrine of unbroken legal transmission.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, textualist_authority_structures, payer,
    institutional, civilizational, identity_locked, global).

% Liberal-democratic states and international human-rights instruments benefit when a major world religion's own internal scholarship supplies a reading that neutralizes politically dangerous textual claims, easing the accommodation of Muslim populations within pluralist legal orders without requiring an external imposition of secular norms.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, secular_pluralist_frameworks, beneficiary,
    institutional, generational, analytical, global).

% Ordinary believers navigating pluralist societies benefit from a reading that relieves them of reconciling a seemingly militant verse with peaceful coexistence, but most lack the classical-Arabic and usul al-fiqh training to adjudicate between this reading and its rivals themselves — they largely receive whichever reading their local authority transmits, and are structurally absent from the scholarly contest that decides which reading circulates.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, lay_muslim_practitioners, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(quran_9_5_scope__progressive_synthesis, lay_muslim_practitioners, excluded).

% Under this reading, groups the verse's literal text addresses (mushrikun) are not construed as a standing class subject to an active directive of war or submission in the present — the constraint exits active operation with respect to them entirely. They have no seat in the scholarly debate but are the party whose exposure to the verse's literal force is most directly at stake in which reading prevails.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, polytheist_and_pagan_communities_historical_and_contemporary, beneficiary,
    powerless, civilizational, analytical, global).

% Movements that ground political-military legitimacy in the abrogating_universal reading are not party to this reading's discourse community and would reject its premises outright; they are structurally excluded from a hermeneutic conversation whose outcome, if widely adopted, would strip the classical-legal warrant they invoke.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, islamist_political_movements, excluded,
    organized, generational, trapped, global).

% Study the historical development of tafsir traditions, the politics of abrogation doctrine, and the sociology of which reading gains institutional traction where, without a stake in which reading wins.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies a framework internal to Islamic scholarship (rather than an externally imposed secular override) for reconciling a text with apparent militant political content with contemporary pluralist coexistence, allowing believers to remain textually and theologically anchored while living under religiously plural, non-theocratic legal orders.
% TRANSFER_FUNCTION: Moves interpretive authority and legitimacy away from institutions whose standing depends on classical abrogation doctrine (naskh) and unbroken literalist transmission, toward scholars and institutions credentialed in historical-critical and trajectory-based hermeneutics; correspondingly moves reputational and political capital toward pluralist religious establishments and away from textualist ones.
% ABSENT_VOICES: The historical and contemporary communities the verse's literal text addresses (mushrikun/polytheists) have no voice in a debate conducted entirely within Muslim scholarly and political communities. Islamist movements grounding legitimacy in the abrogating_universal reading are also absent from this reading's discourse community, having rejected its premises and methods outright.
% DISAPPEARANCE_RATIONALE: If the progressive-synthesis reading vanished from circulation, textualist and abrogationist readings would not automatically fill the vacuum uncontested — the contextual_defensive reading would likely absorb much of its institutional space, since both readings converge on denying the verse standing universal force even though they differ on mechanism (historical closure vs. treaty-specific scope). Reformist institutions built specifically around trajectory hermeneutics would lose a load-bearing argument, and secular-pluralist accommodation of Islam would lose one of its internal-to-the-tradition justifications, but the practical downstream effect on lay practice is contested rather than sharply discontinuous.
% FOUNDING_PROBLEM: Reconciling a scripturally central verse with apparently militant, absolutist content against post-Enlightenment expectations of religious pluralism, human rights, and the illegitimacy of forced conversion or perpetual holy war as state policy — a problem sharpened by 20th/21st-century political movements invoking the verse's literal abrogationist reading to justify contemporary violence.
% FOUNDING_PROBLEM_CORROBORATION: Historians of tafsir (including non-Muslim academic scholars of Islamic intellectual history) corroborate that trajectory/progressive hermeneutics is a genuine, centuries-younger methodological departure responding to real modern political pressure, not merely a post-hoc rationalization invented by the beneficiary community alone — though these same historians note the abrogating_universal and contextual_defensive readings have their own centuries of independent scholarly lineage and are not simply artifacts this reading supersedes by scholarly consensus. No party outside contemporary reformist and pluralist circles asserts the founding problem is fully resolved; classical seminaries dispute that it was ever a problem requiring this solution.
narrative_ontology:disappearance_verdict(quran_9_5_scope__progressive_synthesis, contested).
narrative_ontology:founding_problem_status(quran_9_5_scope__progressive_synthesis, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_9_5_scope__progressive_synthesis, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quran_9_5_scope__progressive_synthesis, 'none', 1).
narrative_ontology:epsilon_provenance(quran_9_5_scope__progressive_synthesis, 0.42, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored moderate (0.42, rising slightly over 1970-2025) reflecting that this reading itself extracts relatively little in material terms but does displace institutional legitimacy and resource flows away from textualist structures over time as the reading gains academic and interfaith institutional traction. Suppression (0.55) reflects real resistance this reading faces from classical seminaries and abrogationist movements, not coercive enforcement by the reading's proponents — this is a scholarly-political contest, not an enforced regime. Theater ratio (0.3) is moderate: some of the reading's institutional life is genuine trajectory-hermeneutic scholarship, some is performative interfaith-dialogue signaling. Accessibility collapse is low-moderate (0.35) — the sibling readings remain fully available and actively contested; this reading has not collapsed alternatives, it competes with them. Resistance is high (0.78) precisely because textualist and abrogationist authority structures actively contest this reading's legitimacy rather than acquiescing to it.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting reformist-scholar seat, this reading is a coordination achievement: it reconciles scripture with pluralist coexistence without requiring external secular imposition. From the textualist-authority payer seat, the same reading is read as an extraction of interpretive legitimacy — a redistribution of scholarly and political authority away from classical transmission-based institutions toward historical-critical methodology, dressed as hermeneutic progress. The engine computing these as different per-seat types from the same structural data is the expected and intended output; the claimed_type (piton) is authored independently of this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformist scholars and secular-pluralist institutions are declared beneficiaries because the reading's spread directly increases their standing and reduces friction for their projects; their d sits toward the beneficiary end. Textualist authority structures are declared the victim group because the reading, if it succeeds institutionally, directly erodes the abrogation-doctrine-dependent legitimacy on which they are built; identity_locked exit reflects that their institutional identity is fused with unbroken-transmission doctrine, not merely inconvenienced by a rival argument. Lay practitioners and the historical/contemporary polytheist referent class are beneficiaries in structural effect (the verse's coercive force is read as inapplicable to them) but have no voice in producing the reading — hence dual beneficiary/excluded roles.
 *
 * MANDATROPHY ANALYSIS:
 *   The claimed_type is piton rather than rope or tangled_rope because, on this reading's own terms, the verse's original coordination function (managing a specific 7th-century treaty-breach crisis) is long dead — the founding_problem_status is contested precisely because reformist scholars say the crisis-management function ended in the 7th century while textualist structures deny the function ever expired. What persists institutionally around the verse (ongoing tafsir industry, seminary curricula, political invocation) is, on the progressive-synthesis account, substantially performative maintenance of a directive whose active period has closed — administered by textualist authority_structures (named agenda-adjacent payer here, though structurally they are also the constraint's chief maintainers in the sibling readings) who bear reputational cost from the reading's spread but for whom abandoning the doctrine of ongoing bindingness is prohibitively costly to their own legitimacy. No concentrated beneficiary captures rents from the verse's continued operative status under this reading — if anything, this reading itself removes the coordination/extraction structure by declaring the constraint inactive, which is exactly why theater_ratio and the piton framing (rather than snare) is appropriate: what would be a captured extraction structure under abrogating_universal is, under this reading, an inertially maintained but functionally hollow doctrinal position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    trajectory_hermeneutic_legitimacy_ambiguity,
    'Is the claim that Quranic ethical trajectory supersedes a specific verse''s literalist application a genuine internal hermeneutic method with classical precedent, or a modern methodological innovation retrofitted to solve a contemporary political-legitimacy problem?',
    'Comparative textual-historical analysis of premodern tafsir literature for genuine precedent of trajectory-based supersession versus post-19th-century reformist innovation; corroboration from historians of Islamic intellectual thought outside both the reformist and textualist camps.',
    'If trajectory hermeneutics is shown to be substantially a modern innovation without deep classical precedent, the reading''s claimed authority (grounded in continuity with the interpretive tradition) weakens relative to its rivals, though this would not by itself refute the reading''s normative claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trajectory_hermeneutic_legitimacy_ambiguity, conceptual, 'Whether progressive-synthesis hermeneutics has genuine classical roots or is a modern methodological innovation.').

omega_variable(
    beneficiary_capture_of_pluralist_framing,
    'Does the progressive_synthesis reading genuinely reflect internal theological development, or is it partly shaped by external pressure from secular-pluralist states and institutions seeking a religiously legitimated accommodation narrative?',
    'Track funding sources, institutional affiliations, and publication venues of leading progressive_synthesis scholars; assess correlation with state-sponsored interfaith and counter-extremism initiatives.',
    'If substantially externally incentivized, the reading''s claim to represent organic internal Quranic-ethics development is weakened, and its classification shifts closer to an externally-subsidized coordination mechanism rather than autonomous theological reasoning.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_capture_of_pluralist_framing, empirical, 'Whether external secular-pluralist incentives shape the reading''s production and institutional support.').

omega_variable(
    kernel_framing_alternative_reading_boundary,
    'Is the three-way split (abrogating_universal / contextual_defensive / progressive_synthesis) the correct decomposition of the kernel, or does contextual_defensive and progressive_synthesis actually collapse into one reading with differing rhetorical emphasis but the same practical legal conclusion (verse not currently operative as universal law)?',
    'Compare the two readings'' treatment of edge cases (e.g., a contemporary state actor claiming treaty-breach grounds for military action against a non-Muslim polity) — if they diverge in practical verdict, the three-way split holds; if they converge, they may be a single reading under two labels.',
    'If contextual_defensive and progressive_synthesis converge on all practical verdicts, this story''s classification as ε-invariant and structurally distinct from contextual_defensive would need re-examination, though the two readings'' account of the verse''s ongoing legal status (dormant-but-potentially-live vs. permanently superseded) differs in ways with real downstream consequence for extreme edge cases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_alternative_reading_boundary, conceptual, 'Whether progressive_synthesis and contextual_defensive are genuinely distinct readings or converge in practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_9_5_scope__progressive_synthesis, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t1970, quran_9_5_scope__progressive_synthesis, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(qura_tr_t1985, quran_9_5_scope__progressive_synthesis, theater_ratio, 1985, 0.22).
narrative_ontology:measurement(qura_tr_t1995, quran_9_5_scope__progressive_synthesis, theater_ratio, 1995, 0.25).
narrative_ontology:measurement(qura_tr_t2005, quran_9_5_scope__progressive_synthesis, theater_ratio, 2005, 0.27).
narrative_ontology:measurement(qura_tr_t2015, quran_9_5_scope__progressive_synthesis, theater_ratio, 2015, 0.29).
narrative_ontology:measurement(qura_tr_t2025, quran_9_5_scope__progressive_synthesis, theater_ratio, 2025, 0.3).

% Extraction over time
narrative_ontology:measurement(qura_be_t1970, quran_9_5_scope__progressive_synthesis, base_extractiveness, 1970, 0.28).
narrative_ontology:measurement(qura_be_t1985, quran_9_5_scope__progressive_synthesis, base_extractiveness, 1985, 0.32).
narrative_ontology:measurement(qura_be_t1995, quran_9_5_scope__progressive_synthesis, base_extractiveness, 1995, 0.36).
narrative_ontology:measurement(qura_be_t2005, quran_9_5_scope__progressive_synthesis, base_extractiveness, 2005, 0.4).
narrative_ontology:measurement(qura_be_t2015, quran_9_5_scope__progressive_synthesis, base_extractiveness, 2015, 0.41).
narrative_ontology:measurement(qura_be_t2025, quran_9_5_scope__progressive_synthesis, base_extractiveness, 2025, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(quran_9_5_scope__progressive_synthesis, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_9_5_scope__progressive_synthesis, identity_coordination).
narrative_ontology:boltzmann_floor_override(quran_9_5_scope__progressive_synthesis, 0.08).
narrative_ontology:affects_constraint(quran_9_5_scope__progressive_synthesis, quran_9_5_scope__abrogating_universal).
narrative_ontology:affects_constraint(quran_9_5_scope__progressive_synthesis, quran_9_5_scope__contextual_defensive).

% DUAL FORMULATION NOTE:
% This story is one of three constraint stories decomposing the natural-language concept 'the scope of Quran 9:5' per the ε-invariance principle. Each reading (abrogating_universal, contextual_defensive, progressive_synthesis) has a distinct ε, distinct beneficiary/victim structure, and distinct claimed_type, because the underlying legal-theological claim about the verse's present binding force differs structurally, not merely rhetorically, across readings. All three are linked bidirectionally via affects_constraints to preserve the kernel-family relationship for contamination-propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
