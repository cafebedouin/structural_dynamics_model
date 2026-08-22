% ============================================================================
% CONSTRAINT STORY: quran_9_5_scope__contextual_defensive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_9_5_scope__contextual_defensive, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: quran_9_5_scope__contextual_defensive
 *   human_readable: Contextual-Defensive Reading of Verse 9:5 (At-Tawbah 'Sword Verse')
 *   domain: Islamic Jurisprudence / Hermeneutics / Political Theology
 *
 * SUMMARY:
 *   This story instantiates the contextual-defensive reading of the so-called
 *   'Sword Verse' (Quran 9:5), one of three structurally distinct readings of
 *   a single contested kernel — the scope of Quran 9:5. This reading holds
 *   that the verse addresses a bounded historical class (treaty-breaking
 *   Meccan-allied polytheist tribes circa 630 CE), does not abrogate the
 *   Quran's peaceful and coexistence-oriented verses, and subordinates the
 *   verse's harsh language to the overriding principles of treaty fidelity
 *   and defensive-only warfare. On this reading's own terms, extraction is
 *   low: the victim class is narrow, historically closed, and defined by
 *   prior aggression (treaty violation), not by religious identity as such.
 *   This is NOT a claim that the abrogating-universal or
 *   progressive-synthesis siblings are wrong — each is authored as its own
 *   constraint with its own ε, beneficiaries, and victim set. The three
 *   readings are linked via network.affects_constraints and share the
 *   kernel_id quran_9_5_scope.
 *
 * KEY AGENTS:
 *   - integrationist_muslim_majority_states: primary beneficiary (institutional/arbitrage) — draws constitutional legitimacy from this reading
 *   - religious_minorities_under_muslim_governance: primary beneficiary (powerless/constrained) — security depends on the narrow victim-set reading holding
 *   - treaty_violating_combatant_tribes_7th_century: sole historical victim class (powerless/trapped) — defines the bounded scope, no longer a live party
 *   - classical_and_contemporary_exegetes_contextualist: agenda_setter (institutional/analytical) — administers the interpretive tradition
 *   - advocates_of_abrogating_universal_reading: excluded rival (organized/analytical) — holds the sibling reading this constraint does not incorporate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_9_5_scope__contextual_defensive, 0.28).
domain_priors:suppression_score(quran_9_5_scope__contextual_defensive, 0.35).
domain_priors:theater_ratio(quran_9_5_scope__contextual_defensive, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, extractiveness, 0.28).
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_9_5_scope__contextual_defensive, rope).
narrative_ontology:human_readable(quran_9_5_scope__contextual_defensive, "Contextual-Defensive Reading of Verse 9:5 (At-Tawbah 'Sword Verse')").
narrative_ontology:topic_domain(quran_9_5_scope__contextual_defensive, "Islamic Jurisprudence / Hermeneutics / Political Theology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_9_5_scope__contextual_defensive, 'cca0d6e0-7e68-4ed8-8ada-d0dd1f03df49').
narrative_ontology:cs_kernel_codification('cca0d6e0-7e68-4ed8-8ada-d0dd1f03df49', fixed_text).
narrative_ontology:cs_authority_grounding('cca0d6e0-7e68-4ed8-8ada-d0dd1f03df49', lineage).
narrative_ontology:cs_interpretation_layer_present('cca0d6e0-7e68-4ed8-8ada-d0dd1f03df49').
narrative_ontology:cs_reading_relation('cca0d6e0-7e68-4ed8-8ada-d0dd1f03df49', quran_9_5_scope__abrogating_universal, forecloses).
narrative_ontology:cs_reading_relation('cca0d6e0-7e68-4ed8-8ada-d0dd1f03df49', quran_9_5_scope__progressive_synthesis, coexists_with).
narrative_ontology:cs_axiom('cca0d6e0-7e68-4ed8-8ada-d0dd1f03df49', foundational, occasion_bound_scope_limits_command).
narrative_ontology:cs_axiom_status(occasion_bound_scope_limits_command, holdable).
narrative_ontology:cs_axiom_grounding('cca0d6e0-7e68-4ed8-8ada-d0dd1f03df49', occasion_bound_scope_limits_command, conventional).
narrative_ontology:cs_axiom('cca0d6e0-7e68-4ed8-8ada-d0dd1f03df49', foundational, treaty_fidelity_overrides_general_warfare_license).
narrative_ontology:cs_axiom_status(treaty_fidelity_overrides_general_warfare_license, holdable).
narrative_ontology:cs_axiom_grounding('cca0d6e0-7e68-4ed8-8ada-d0dd1f03df49', treaty_fidelity_overrides_general_warfare_license, deontological).
narrative_ontology:cs_reference_frame('cca0d6e0-7e68-4ed8-8ada-d0dd1f03df49', medinan_treaty_context_primacy).
narrative_ontology:cs_drift_state('cca0d6e0-7e68-4ed8-8ada-d0dd1f03df49', post_colonial_political_islam_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('cca0d6e0-7e68-4ed8-8ada-d0dd1f03df49', '').
narrative_ontology:cs_kernel_id(quran_9_5_scope__contextual_defensive, quran_9_5_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, integrationist_muslim_majority_states).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, religious_minorities_under_muslim_governance).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, muslim_communities_in_pluralist_societies).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, interfaith_coexistence_advocates).
narrative_ontology:constraint_victim(quran_9_5_scope__contextual_defensive, treaty_violating_combatant_tribes_7th_century).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States with Muslim-majority populations that ground constitutional pluralism, minority protection, and non-aggression foreign policy on this reading. The contextual-defensive reading gives their legal and diplomatic institutions a scriptural basis for treaty-respecting, non-expansionist governance rather than having to defend against a universal-warfare reading of their own foundational texts.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, integrationist_muslim_majority_states, beneficiary,
    institutional, generational, arbitrage, national).

% Non-Muslim populations living under Muslim-majority political authority. Under this reading, they are structurally exempted from the verse's scope because the verse's victim set is limited to treaty-breaking combatants, not religious out-groups as such. Their security depends on this reading holding institutional ground against rival readings that would broaden the category of legitimate target.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, religious_minorities_under_muslim_governance, beneficiary,
    powerless, biographical, constrained, national).

% Muslim minority populations living in non-Muslim-majority states who face public suspicion tied to competing interpretations of this verse. This reading gives them an internally coherent theological basis to publicly disavow offensive-jihad readings and to participate in interfaith and civic life without perceived scriptural contradiction.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, muslim_communities_in_pluralist_societies, beneficiary,
    moderate, generational, constrained, global).

% The historically specific polytheist Meccan-allied tribes named in classical exegesis as having repeatedly broken treaty terms with the nascent Medinan polity. Under this reading, they are the sole class the verse addresses, and the verse's harsh language is read as a time-and-context-bound response to their specific betrayal, not as a template for indefinite future application. As a historical class they no longer exist to exit or contest the reading; their inclusion here is structural (they define the victim set) rather than a live grievance.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, treaty_violating_combatant_tribes_7th_century, payer,
    powerless, immediate, trapped, local).

% Scholars, jurists, and institutions (e.g., al-Azhar-aligned bodies, contextualist tafsir traditions) who administer and transmit this reading through fatwa, curriculum, and public theology. They set the interpretive terms by which occasion-of-revelation (asbab al-nuzul) evidence is weighted against the abrogation (naskh) doctrine favored by rival readings.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, classical_and_contemporary_exegetes_contextualist, agenda_setter,
    institutional, civilizational, analytical, global).

% Scholars and movements holding that 9:5 abrogates prior peaceful verses and establishes a standing offensive obligation. They are structurally excluded from this reading's own internal coherence — not silenced by force, but their premise (blanket abrogation) is treated as textually and historically unsupported within this reading's interpretive framework. They would object that this reading domesticates a command they read as binding and unconditional.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, advocates_of_abrogating_universal_reading, excluded,
    organized, civilizational, analytical, global).

% State and non-state actors who invoke a maximalist reading of 9:5 to legitimate ongoing or offensive military campaigns against non-Muslim or rival-Muslim populations. This reading structurally denies them the scriptural warrant they seek; they are excluded from the reading's coordination logic because their claimed application (open-ended offensive war) falls outside the treaty-violation/defensive scope this reading recognizes.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, governments_citing_verse_for_conflict_justification, excluded,
    powerful, biographical, constrained, regional).

% Historians and religious studies academics who evaluate the asbab al-nuzul record, classical tafsir chains, and the historical treaty context (Hudaybiyyah and its collapse) against competing readings without institutional stake in any one outcome.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_9_5_scope__contextual_defensive, diffuse).
narrative_ontology:fixing_cost_class(quran_9_5_scope__contextual_defensive, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Anchors the verse's harsh wartime language to its specific historical occasion (treaty-breaking by named Meccan-allied tribes in 630 CE), preserving the Quran's numerous peace-and-coexistence verses as governing norms and treaty obligation as the controlling legal principle for relations with non-Muslims, rather than reading 9:5 as abrogating them.
% TRANSFER_FUNCTION: Transfers interpretive legitimacy and legal-theological authority away from readings that would license indefinite offensive warfare, toward readings that ground non-aggression, treaty-fidelity, and minority protection. No material resource is transferred; what moves is the scope of scripturally sanctioned violence — narrowed from 'polytheists generally' to 'treaty violators specifically.'
% ABSENT_VOICES: The historical treaty-violating tribes cannot speak for themselves and are known only through classical Arabic exegetical sources compiled generations later, raising an unresolved historiographical question about how reliably the asbab al-nuzul narrative reconstructs their actual conduct. Advocates of the abrogating-universal reading are present in the broader discourse but structurally excluded from this reading's own coordination logic.
% DISAPPEARANCE_RATIONALE: If the contextual-defensive reading lost institutional and scholarly ground entirely, states and communities that ground pluralist governance and minority protection on it would lose their strongest scriptural counter to maximalist readings; interfaith coexistence frameworks in several Muslim-majority constitutional orders would need new theological grounding, and minority communities' legal security would become more contested.
% FOUNDING_PROBLEM: Classical exegetes needed to reconcile an apparently unconditional command ('kill the polytheists wherever you find them') with the Quran's own extensive peace, coexistence, and non-compulsion verses (e.g., 2:256, 60:8) and with the specific historical record of the Hudaybiyyah treaty's collapse — the founding problem was internal scriptural and legal coherence, not merely modern apologetics.
% FOUNDING_PROBLEM_CORROBORATION: Classical tafsir authorities (e.g., al-Tabari's transmission chains recording the Hudaybiyyah context) and independent historians of early Islam outside any modern advocacy tradition corroborate the asbab al-nuzul record placing the verse in the treaty-collapse context. Advocates of the abrogating-universal reading, working from within a distinct classical naskh tradition, dispute that this context limits the verse's legal scope — the corroboration for context is real but its legal-binding force is exactly what remains contested across the kernel.
narrative_ontology:disappearance_verdict(quran_9_5_scope__contextual_defensive, world_rearranges).
narrative_ontology:founding_problem_status(quran_9_5_scope__contextual_defensive, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_9_5_scope__contextual_defensive, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quran_9_5_scope__contextual_defensive, 'none', 1).
narrative_ontology:epsilon_provenance(quran_9_5_scope__contextual_defensive, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_9_5_scope__contextual_defensive_tests).
:- end_tests(quran_9_5_scope__contextual_defensive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.28) because, under this reading's own lights, the constraint licenses violence only against a historically closed class that itself initiated treaty-breaking aggression — there is no ongoing, generalizable target group. Suppression is moderate (0.35): reflects the real, sustained scholarly and political effort required to hold this reading against maximalist counter-readings, not coercion of a victim population. Theater ratio is modest and drifts slightly upward over the measured interval (0.15 to 0.22), reflecting increasing performative citation of 'moderate Islam' framing in state and diplomatic contexts without a corresponding change in the underlying exegetical substance — a mild Goodhart signal worth tracking, not a reclassification trigger. Resistance is authored higher (0.55) because this reading is actively contested by rival scholarly traditions and by political actors who benefit from the maximalist reading, meaning defending this reading's ground requires ongoing argumentative and institutional work.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of integrationist Muslim-majority states and minority communities, this reading operates as coordination: a shared, narrow, defensible scope that protects pluralism. From the seat of advocates holding the abrogating-universal reading, the same textual data is read as illegitimate narrowing of a binding command — they experience this reading's institutional dominance in certain jurisdictions as suppression of their own theological claim, not as coordination. The engine computes these divergent seat classifications from the structural data; this story does not adjudicate which seat is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (integrationist states, religious minorities, diaspora Muslim communities) sit near the beneficiary end of directionality because the reading's narrow scope directly protects their legal and physical security. The sole victim class (historical treaty-violating tribes) sits at the target end by construction — but note this is a closed historical class with no living members, which distinguishes this constraint sharply from a standing extractive arrangement: there is no perpetuation mechanism transferring ongoing costs onto a present-day population. This is why extractiveness stays low despite a nonempty victim declaration — the victim set does not recur.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling an apparently unconditional wartime command with the Quran's peace verses and the specific Hudaybiyyah treaty context) is authored as contested rather than dead: contextualist exegetes hold the founding problem remains genuinely live as an interpretive task, while critics of this reading (from the abrogating-universal side) would say the 'problem' framing itself is a modern importation onto a text they read as always having intended universal scope. This divergence is exactly why founding_problem_status is 'contested' rather than 'live' or 'dead' — collapsing it to either would misrepresent the genealogy dispute as settled when it is the kernel's central fault line.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    asbab_al_nuzul_reliability,
    'How reliably do the classical occasion-of-revelation (asbab al-nuzul) narratives, compiled generations after the events, reconstruct the actual historical conduct of the named treaty-breaking tribes?',
    'Comparative historiographical analysis of isnad (transmission chain) reliability across competing tafsir traditions, cross-referenced against independent early Islamic historical sources where they exist.',
    'If the asbab al-nuzul record is judged historically unreliable, the contextual-defensive reading''s core evidentiary basis weakens, strengthening the abrogating-universal reading''s claim that context-limitation is exegetically motivated rather than historically grounded.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(asbab_al_nuzul_reliability, empirical, 'Reliability of the historical context narrative this reading depends on.').

omega_variable(
    kernel_reading_selection,
    'This constraint instantiates the contextual_defensive reading of the quran_9_5_scope kernel. The sibling readings (abrogating_universal, progressive_synthesis) are authored as separate constraints with their own epsilon, beneficiaries, and victim sets. What determines which reading a given interpretive community, state, or individual adopts, and is that selection itself doctrinally principled or politically contingent?',
    'Cross-tradition comparative study of which schools/eras adopted naskh (abrogation) doctrine broadly vs. narrowly, and correlation with the political conditions (expansionist vs. defensive state postures) under which each reading gained institutional dominance.',
    'If reading-selection correlates strongly with the political interests of the adopting authority (expansionist states favor abrogating_universal; pluralist states favor contextual_defensive), that would support treating the kernel contest itself as partly downstream of political economy rather than purely doctrinal — without resolving which reading is textually correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection, conceptual, 'What determines selection among the three sibling readings of the kernel.').

omega_variable(
    naskh_doctrine_scope_ambiguity,
    'Is the classical doctrine of naskh (abrogation) itself best understood as applying to specific ritual/legal rulings only, or as a general hermeneutic principle capable of overriding entire categories of prior verses (including peace and coexistence verses)?',
    'Systematic review of classical usul al-fiqh (jurisprudential theory) texts on the scope conditions for naskh, independent of any single verse''s application.',
    'A narrow naskh doctrine supports this reading''s claim that 9:5 cannot abrogate the broad category of peace verses; a broad naskh doctrine would lend more support to the abrogating_universal sibling reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naskh_doctrine_scope_ambiguity, conceptual, 'Scope of the abrogation doctrine underlying the kernel dispute.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_9_5_scope__contextual_defensive, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_9_5_scope__contextual_defensive, theater_ratio, 0, 0.15).
narrative_ontology:measurement(qura_tr_t8, quran_9_5_scope__contextual_defensive, theater_ratio, 8, 0.16).
narrative_ontology:measurement(qura_tr_t16, quran_9_5_scope__contextual_defensive, theater_ratio, 16, 0.18).
narrative_ontology:measurement(qura_tr_t24, quran_9_5_scope__contextual_defensive, theater_ratio, 24, 0.19).
narrative_ontology:measurement(qura_tr_t32, quran_9_5_scope__contextual_defensive, theater_ratio, 32, 0.21).
narrative_ontology:measurement(qura_tr_t40, quran_9_5_scope__contextual_defensive, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_9_5_scope__contextual_defensive, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(qura_be_t8, quran_9_5_scope__contextual_defensive, base_extractiveness, 8, 0.23).
narrative_ontology:measurement(qura_be_t16, quran_9_5_scope__contextual_defensive, base_extractiveness, 16, 0.25).
narrative_ontology:measurement(qura_be_t24, quran_9_5_scope__contextual_defensive, base_extractiveness, 24, 0.26).
narrative_ontology:measurement(qura_be_t32, quran_9_5_scope__contextual_defensive, base_extractiveness, 32, 0.27).
narrative_ontology:measurement(qura_be_t40, quran_9_5_scope__contextual_defensive, base_extractiveness, 40, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(quran_9_5_scope__contextual_defensive, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_9_5_scope__contextual_defensive, identity_coordination).
narrative_ontology:affects_constraint(quran_9_5_scope__contextual_defensive, quran_9_5_scope__abrogating_universal).
narrative_ontology:affects_constraint(quran_9_5_scope__contextual_defensive, quran_9_5_scope__progressive_synthesis).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the quran_9_5_scope kernel. abrogating_universal claims the verse abrogates prior peaceful verses and establishes standing offensive obligation (high extraction, broad and recurring victim set: all non-Muslims resisting submission). contextual_defensive (this story) claims the verse addresses a bounded historical class only, extraction is low, and the victim set is closed and historical. progressive_synthesis claims the verse is a time-bound political directive superseded by the Quran's broader ethical trajectory, effectively dissolving any standing legal command entirely. The three stories share no ε value by design — each reading produces a structurally distinct constraint from the same text, per the ε-invariance principle. They are linked here rather than merged because merging would force an artificial averaging across readings that the framework's authoring rule explicitly prohibits.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
