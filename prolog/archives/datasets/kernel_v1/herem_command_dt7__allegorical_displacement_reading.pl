% ============================================================================
% CONSTRAINT STORY: herem_command_dt7__allegorical_displacement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_herem_command_dt7__allegorical_displacement_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: herem_command_dt7__allegorical_displacement_reading
 *   human_readable: Herem Command (Allegorical Displacement Reading): Internal Spiritual Warfare Coordination
 *   domain: biblical_hermeneutics/religious_ethics/commitment_system
 *
 * SUMMARY:
 *   The Dt7 herem command (Deuteronomy 7:1-2: 'When the LORD your God brings
 *   you into the land and clears away many nations before you...you must put
 *   them to the ban [herem]...') presents a structural constraint for
 *   post-biblical readers committed to both textual authority and universal
 *   human dignity. The allegorical displacement reading resolves this by
 *   relocating the constraint entirely from the interethnic domain to the
 *   internal spiritual domain: the 'nations' are reinterpreted as internal
 *   vices (sin, idolatry, temptation), and 'conquest' becomes metaphorical
 *   self-discipline and moral transformation. This reading emerges in
 *   patristic exegesis (Origen, Augustine), develops through medieval
 *   allegory (Aquinas, Jewish mysticism), and becomes dominant in Protestant
 *   reformation theology (Calvin, Beza) where it coordinates a hermeneutical
 *   framework allowing retention of the text as authoritative while
 *   evacuating its literal interethnic implications. The constraint's low
 *   extractiveness (0.08) reflects that the reading functions primarily as
 *   coordination (shared hermeneutical vocabulary enabling cross-generational
 *   theological communication) with minimal coercive suppression — exit is
 *   available via alternative readings or theological frameworks. The theater
 *   ratio (0.35) reflects moderate performative content: the metaphorical
 *   language requires repeated interpretive work to maintain coherence,
 *   especially when applied practitioners try to instantiate 'internal
 *   conquest' in lived spiritual discipline.
 *
 * KEY AGENTS:
 *   - Allegorical Interpretive Community: Organized religious scholars and practitioners (patristic, medieval, Reformed theologians) who adopt and transmit the allegorical reading (organized/mobile)
 *   - Theological Authority: Institutions and authoritative interpreters (church hierarchy, rabbinical lineage, magisterial traditions) who promulgate the reading and establish its legitimacy (powerful/mobile)
 *   - Practicing Believer: Individual adherents seeking coherence between biblical text and moral commitments (moderate/constrained)
 *   - Analytical Observer: Civilizational perspective examining the cognitive necessity of displacement given post-Enlightenment moral principles (analytical/analytical)
 *   - Literal-Reading Community: Agents who maintain durable separation reading (structurally distinct; separate constraint story)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(herem_command_dt7__allegorical_displacement_reading, 0.08).
domain_priors:suppression_score(herem_command_dt7__allegorical_displacement_reading, 0.12).
domain_priors:theater_ratio(herem_command_dt7__allegorical_displacement_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herem_command_dt7__allegorical_displacement_reading, rope).
narrative_ontology:human_readable(herem_command_dt7__allegorical_displacement_reading, "Herem Command (Allegorical Displacement Reading): Internal Spiritual Warfare Coordination").
narrative_ontology:topic_domain(herem_command_dt7__allegorical_displacement_reading, "biblical_hermeneutics/religious_ethics/commitment_system").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__allegorical_displacement_reading, 'c064a514-5b32-4e7f-b4d5-c35c9bd6328c').
narrative_ontology:cs_kernel_codification('c064a514-5b32-4e7f-b4d5-c35c9bd6328c', fixed_text).
narrative_ontology:cs_authority_grounding('c064a514-5b32-4e7f-b4d5-c35c9bd6328c', lineage).
narrative_ontology:cs_interpretation_layer_present('c064a514-5b32-4e7f-b4d5-c35c9bd6328c').
narrative_ontology:cs_reading_relation('c064a514-5b32-4e7f-b4d5-c35c9bd6328c', herem_command_dt7__durable_separation_reading, coexists_with).
narrative_ontology:cs_reading_relation('c064a514-5b32-4e7f-b4d5-c35c9bd6328c', herem_command_dt7__contextual_supersession_reading, influences).
narrative_ontology:cs_axiom('c064a514-5b32-4e7f-b4d5-c35c9bd6328c', foundational, moral_universalism_foundational).
narrative_ontology:cs_axiom_status(moral_universalism_foundational, holdable).
narrative_ontology:cs_axiom_grounding('c064a514-5b32-4e7f-b4d5-c35c9bd6328c', moral_universalism_foundational, deontological).
narrative_ontology:cs_axiom('c064a514-5b32-4e7f-b4d5-c35c9bd6328c', foundational, textual_authority_non_negotiable).
narrative_ontology:cs_axiom_status(textual_authority_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('c064a514-5b32-4e7f-b4d5-c35c9bd6328c', textual_authority_non_negotiable, theological).
narrative_ontology:cs_reference_frame('c064a514-5b32-4e7f-b4d5-c35c9bd6328c', universal_moral_principles_compatibility).
narrative_ontology:cs_drift_state('c064a514-5b32-4e7f-b4d5-c35c9bd6328c', post_enlightenment_ethics, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('c064a514-5b32-4e7f-b4d5-c35c9bd6328c', '').
narrative_ontology:cs_kernel_id(herem_command_dt7__allegorical_displacement_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herem_command_dt7__allegorical_displacement_reading, spiritual_practitioners).
narrative_ontology:constraint_beneficiary(herem_command_dt7__allegorical_displacement_reading, interpretive_community).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ALLEGORICAL INTERPRETIVE COMMUNITY (ROPE) — Organized religious scholars and practitioners who adopt the allegorical reading coordinate around a shared hermeneutical framework. The constraint functions as pure coordination: establishing shared vocabulary (sin, temptation as internal enemies; moral warfare as disciplinary practice) enables communication across generations and traditions. No agent extracts from others — the framework is a collective achievement that all parties benefit from equally. Low suppression because exit is available (different interpretive frameworks exist) and the commitment is primarily epistemic rather than coercive.
constraint_indexing:constraint_classification(herem_command_dt7__allegorical_displacement_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 2: THEOLOGICAL AUTHORITY (ROPE) — Religious institutions or authoritative interpreters (patristic tradition, Reformed theology, rabbinical interpretation) who promulgate the allegorical reading experience it as a coordination mechanism enabling legitimate theological claim-making. The reading provides hermeneutical authority: it allows reframing Dt7 in ways that align with broader theological commitments (universalism, pacifism, internal moral transformation) without requiring denial of the text. Suppression is low because theological authorities have exit options (adopt different readings or generate new interpretations) and the constraint functions through persuasion rather than coercion. Extractiveness is minimal — authorities benefit from the legitimacy the reading provides, but not through extraction from subordinates.
constraint_indexing:constraint_classification(herem_command_dt7__allegorical_displacement_reading, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: PRACTICING BELIEVER (SCAFFOLD) — Individual adherents who adopt the allegorical reading to resolve cognitive dissonance between the Dt7 command (literal conquest violence) and theological commitments (non-violence, universal human dignity). The reading temporarily scaffolds them through a period of textual uncertainty, providing a framework for moral coherence. However, the framework is structurally temporary — it works only as long as alternative interpretive paths remain available and the believer's theological commitments can sustain the metaphorical displacement. Suppression is moderate: if the reading becomes mandatory, it converts from scaffold to constraint. Exit is constrained but available: the believer can adopt different readings (durable separation, contextual supersession) or revise theological commitments, at a cost to community standing.
constraint_indexing:constraint_classification(herem_command_dt7__allegorical_displacement_reading, scaffold,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER / NATURAL LIMIT VIEW (MOUNTAIN) — From a civilizational analytical perspective, the allegorical displacement reading responds to an immutable cognitive constraint: human moral sensibilities cannot consistently hold both the Dt7 command (ethnic conquest) and the principle of universal human dignity in the same framework simultaneously. Displacement into the internal spiritual domain is not a choice but a structural necessity — any framework that attempts to honor both the text and modern moral principles must relocate the constraint's application domain. This perspective classifies as mountain because the cognitive limit appears unchangeable: the principle of universal dignity is foundational to post-Enlightenment ethics, and literal conquest morality is incompatible with it. However, this risks false-summit classification — the 'immutability' may reflect contingent historical developments rather than natural law.
constraint_indexing:constraint_classification(herem_command_dt7__allegorical_displacement_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(herem_command_dt7__allegorical_displacement_reading_tests).
:- end_tests(herem_command_dt7__allegorical_displacement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The allegorical displacement reading functions as pure coordination — it establishes a shared hermeneutical framework allowing interpreters to honor both textual authority and moral universalism. No agent extracts from others through this reading; instead, all parties benefit from the clarity and coherence it provides. The low value reflects zero extractiveness on interethnic relations (the victim set collapses from ethnic groups to abstract vices). Compare to the durable separation reading (separate constraint), which would have higher extractiveness because it assigns concrete interethnic benefits/costs. Suppression (0.12): Very low. The reading does not suppress alternatives — literal readings, contextual supersession, and other hermeneutical frameworks remain available and are actively used. Practitioners can exit this reading by adopting different frameworks, at the cost of community standing but without material penalty. Theater ratio (0.35): Moderate-low. The reading requires genuine interpretive labor to apply (translating 'conquest' to 'self-discipline,' 'nations' to 'vices'), but this labor is substantive rather than purely performative. The reading has functioned across 1,800 years and across multiple theological traditions (Catholic, Orthodox, Protestant, Jewish), suggesting it provides genuine hermeneutical value beyond theatrical maintenance.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives except the analytical observer converge on Rope or Scaffold classifications, indicating minimal perspectival gap. This convergence is diagnostic: when a constraint shows unanimous Rope classification across different power levels (organized, powerful, moderate), it indicates genuine coordination function with no embedded extraction. The analytical observer's Mountain perspective risks false summitry — it naturalizes the cognitive displacement as immutable when the displacement actually reflects contingent historical developments (rise of universal human rights discourse in Enlightenment). The gap reveals that what appears as natural law (cognitive necessity of displacement) is actually a reading chosen within a commitment system that could generate alternative readings with different victim sets and extractiveness profiles.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint operates at the organizational and institutional power levels, not the powerless level. Primary beneficiaries are (1) theological authorities who gain hermeneutical legitimacy and interpretive authority by promulgating the reading, and (2) practicing believers who gain cognitive coherence by adopting it. Neither faces suppression — both have mobile or arbitrage exit options (can adopt alternative readings, can revise theological commitments). The analytical observer faces no directionality at all: they are examining the constraint from outside the commitment system. No victim group exists in this reading — there is no structural asymmetry that would produce extraction. This distinguishes it sharply from the durable separation reading, which would have clear victims (excluded ethnic groups) and would show d values favoring the separation-beneficiaries. The low d values across all perspectives reflect the coordination-only nature of this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading is NOT subject to mandatrophy because extractiveness (0.08) is well below the threshold (0.70) where mandatrophy gates activate. The classification as Rope is stable and non-paradoxical. However, the constraint does resolve an apparent paradox at the kernel level: the Dt7 text can be simultaneously authoritative AND consistent with universal moral principles, if the reading displaces its reference domain. The mandatrophy that would apply is at the kernel level (the contested text itself), not at this reading's constraint level. Each reading of the kernel will have different extractiveness and different classification — the durable separation reading will show high extractiveness on interethnic relations, making it a Tangled Rope or Snare; the contextual supersession reading will show zero extractiveness because it categorizes the command as historically superseded.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_foreclosure_scope,
    'Does the allegorical displacement reading logically foreclose the durable separation reading, or do they coexist as live interpretive options?',
    'Textual analysis of whether allegorical displacement requires treating separation reading''s foundational claims as incoherent or merely false. Review patristic and medieval exegesis to document which readings were held as simultaneously live vs. mutually exclusive.',
    'If forecloses: the sibling relationship is ''forecloses'' (this reading''s core premise makes separation reading logically impossible). If coexists: the relationship is ''coexists_with'' (both remain live options for different interpreters). This changes the constraint''s structural relationship to its siblings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_scope, conceptual, 'Whether allegorical displacement logically forecloses durable separation reading').

omega_variable(
    metaphorical_violence_phenomenology,
    'Does the metaphorical reframing of ''conquest'' as internal moral warfare constitute a genuine alternative constraint or merely a reinterpretation of the same constraint?',
    'Analysis of what changes in lived practice and institutional structure when the reading shifts from literal to metaphorical. If the same discipline structures, authority relations, and enforcement mechanisms persist under metaphorical language, the reframing may be purely terminological (same constraint, new label). If practice and enforcement change substantively, a genuinely different constraint has been instantiated.',
    'If merely reinterpretation: this story and the literal conquest story are different perspectives on one constraint, not distinct constraints. If genuinely alternative: the low extractiveness (0.08) correctly reflects the displacement, and the constraint genuinely changes when the reading changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metaphorical_violence_phenomenology, empirical, 'Whether metaphorical reframing constitutes genuinely different constraint or terminological relabeling').

omega_variable(
    kernel_textual_ambiguity,
    'Is the Dt7 command''s textual ambiguity inherent (the text itself supports multiple readings) or projected (interpreters impose ambiguity onto a univocal text)?',
    'Close reading of Dt7 syntax, vocabulary, and narrative context to determine whether textual features genuinely support allegorical displacement or whether the reading requires departing from the text''s plain sense. Examine parallel passages and genre conventions to establish what ''conquest'' meant in Iron Age Levantine literature.',
    'If inherent ambiguity: multiple readings (including allegorical displacement) are legitimate textual interpretations; the kernel genuinely admits multiple readings. If projected: allegorical displacement is a theological imposition on a univocal text; the reading''s legitimacy depends on authority structures rather than textual features.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_textual_ambiguity, empirical, 'Whether Dt7 command''s textual ambiguity is inherent or projected').

omega_variable(
    authority_structure_dependency,
    'Does the allegorical displacement reading depend on particular authority structures (institutional church, rabbinical lineage, magisterium) to sustain its legitimacy, or does it stand as a self-defending interpretation?',
    'Track adoption and abandonment of the reading across contexts where authoritative structures were weak, contested, or absent. Examine whether the reading persists in independent interpretive communities or only where institutional authority enforces it.',
    'If authority-dependent: the constraint''s low suppression (0.12) may underestimate institutional coercion embedded in how the reading is transmitted. If self-defending: the low suppression correctly reflects the reading''s rational persuasiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_structure_dependency, empirical, 'Extent to which allegorical displacement reading depends on institutional authority for legitimacy').

omega_variable(
    universal_victim_collapse_completeness,
    'Does the displacement to internal spiritual warfare domains completely eliminate ethical reference to actual human groups (ethnic, national, marginalized communities), or does the metaphorical language still carry implicit interethnic implications?',
    'Examine historical instances where the allegorical reading was applied: did interpreters successfully avoid making claims about actual ethnic groups, or did metaphorical language (e.g., ''Christian soldiers,'' ''spiritual armor'' against external threats) implicitly reconstruct ethnic boundaries at a different level?',
    'If completely eliminates: the zero extractiveness on interethnic relations (0.08 reflects only internal spiritual domain) is accurate. If metaphorical language reconstructs ethnic boundaries: the constraint''s actual extractiveness on interethnic relations may be higher, and the reading functions as displacement rather than elimination of the original constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_victim_collapse_completeness, empirical, 'Whether metaphorical language completely eliminates interethnic implications or reconstructs them indirectly').

omega_variable(
    reading_stability_across_historical_contexts,
    'Has the allegorical displacement reading maintained stable meaning across different historical periods (patristic, medieval, reformation, modern), or has its content shifted substantially in response to changing theological and political contexts?',
    'Comparative analysis of allegorical interpretations across periods: document what ''internal enemies'' (sin, temptation, vice) meant in each context, and whether the metaphorical mapping changed when political contexts changed.',
    'If stable: the reading represents a robust hermeneutical principle with consistent content. If shifting: the reading may function more as a hermeneutical strategy that adapts to maintain compatibility with whatever theological commitments are current, suggesting it operates closer to theater (0.35 may underestimate).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_stability_across_historical_contexts, empirical, 'Stability of allegorical displacement reading''s content across historical periods').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__allegorical_displacement_reading, 0, 1500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(herem_alleg_theater_t0, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(herem_alleg_theater_t500, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 500, 0.32).
narrative_ontology:measurement(herem_alleg_theater_t1500, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 1500, 0.35).

% Extraction over time
narrative_ontology:measurement(herem_alleg_extract_t0, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 0, 0.04).
narrative_ontology:measurement(herem_alleg_extract_t500, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 500, 0.06).
narrative_ontology:measurement(herem_alleg_extract_t1500, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 1500, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(herem_command_dt7__allegorical_displacement_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(herem_command_dt7__allegorical_displacement_reading, 0.06).
narrative_ontology:affects_constraint(herem_command_dt7__allegorical_displacement_reading, herem_command_dt7__durable_separation_reading).
narrative_ontology:affects_constraint(herem_command_dt7__allegorical_displacement_reading, herem_command_dt7__contextual_supersession_reading).

% DUAL FORMULATION NOTE:
% The allegorical displacement reading is one constraint within a three-reading kernel family covering Dt7 herem command. Sibling readings are distinct constraints with different victim sets, different extractiveness values, and different authority groundings. This reading displaces the constraint entirely to internal spiritual domain (extractiveness 0.08, no interethnic victims). The durable separation reading maintains interethnic application (extractiveness significantly higher, ethnic groups as victims). The contextual supersession reading treats the command as historically bounded (extractiveness zero on contemporary groups because command is classified as superseded). Each reading is a clean ε-invariant constraint; the family links via network.affects_constraints showing that adoption of one reading influences interpretive paths available for the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
