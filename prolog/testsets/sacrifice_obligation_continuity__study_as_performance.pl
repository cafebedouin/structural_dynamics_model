% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity__study_as_performance, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sacrifice_obligation_continuity__study_as_performance
 *   human_readable: Sacrifice Obligation Continuity via Textual Engagement (Study-as-Performance Reading)
 *   domain: religious/textual tradition
 *
 * SUMMARY:
 *   In Jewish legal tradition, after the Second Temple's destruction in 70
 *   CE, physical sacrifice became impossible. The rabbinic reading
 *   authoritively interprets Deuteronomy 8:3 ('one does not live by bread
 *   alone but by every utterance from God's mouth') to establish that study
 *   and interpretation of sacrifice law fulfills the commandment to offer
 *   sacrifice. This reading transforms an unfulfillable obligation into a
 *   continuously fulfillable one through textual engagement. The reading is
 *   contested: performance-only advocates argue study is merely preparation;
 *   messianic suspension advocates maintain the obligation is deferred;
 *   archival preservationists treat study as cultural preservation without
 *   binding force. This story instantiates ONLY the study-as-performance
 *   reading as a clean, ε-invariant constraint, with its own beneficiary
 *   structure, directionality, and coordination function. The other readings
 *   are separate constraints in the same kernel family, not alternative
 *   perspectives on this one.
 *
 * KEY AGENTS:
 *   - Rabbinic interpreters: Set and enforce the study-as-performance reading; hold institutional authority over what counts as legitimate obligation fulfillment; derive authority and control from interpretive gatekeeping.
 *   - Textual scholars and study practitioners: Benefit from the reading by gaining legitimate performance status through study; organize in interpretive communities; transmit the obligation across generations through textual engagement.
 *   - Non-literate community members: Excluded from direct fulfillment because the reading centers textual engagement; their obligation compliance becomes mediated through scholars' performance.
 *   - Performance-only and messianic advocates: Occupy excluded institutional seats; dispute the core premise of the reading; remain organized in alternative scholarly and devotional communities.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__study_as_performance, 0.18).
domain_priors:suppression_score(sacrifice_obligation_continuity__study_as_performance, 0.12).
domain_priors:theater_ratio(sacrifice_obligation_continuity__study_as_performance, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, extractiveness, 0.18).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, resistance, 0.14).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__study_as_performance, rope).
narrative_ontology:human_readable(sacrifice_obligation_continuity__study_as_performance, "Sacrifice Obligation Continuity via Textual Engagement (Study-as-Performance Reading)").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__study_as_performance, "religious/textual tradition").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__study_as_performance, '74032b5e-dc66-4804-b2cd-e0f1dee150b8').
narrative_ontology:cs_kernel_codification('74032b5e-dc66-4804-b2cd-e0f1dee150b8', fixed_text).
narrative_ontology:cs_authority_grounding('74032b5e-dc66-4804-b2cd-e0f1dee150b8', lineage).
narrative_ontology:cs_interpretation_layer_present('74032b5e-dc66-4804-b2cd-e0f1dee150b8').
narrative_ontology:cs_reading_relation('74032b5e-dc66-4804-b2cd-e0f1dee150b8', sacrifice_obligation_continuity__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('74032b5e-dc66-4804-b2cd-e0f1dee150b8', sacrifice_obligation_continuity__messianic_suspension, coexists_with).
narrative_ontology:cs_reading_relation('74032b5e-dc66-4804-b2cd-e0f1dee150b8', sacrifice_obligation_continuity__archival_preservation, influences).
narrative_ontology:cs_axiom('74032b5e-dc66-4804-b2cd-e0f1dee150b8', foundational, textual_engagement_performs_obligation).
narrative_ontology:cs_axiom_status(textual_engagement_performs_obligation, holdable).
narrative_ontology:cs_axiom_grounding('74032b5e-dc66-4804-b2cd-e0f1dee150b8', textual_engagement_performs_obligation, deontological).
narrative_ontology:cs_axiom('74032b5e-dc66-4804-b2cd-e0f1dee150b8', foundational, study_continuity_preserves_binding_force).
narrative_ontology:cs_axiom_status(study_continuity_preserves_binding_force, holdable).
narrative_ontology:cs_axiom_grounding('74032b5e-dc66-4804-b2cd-e0f1dee150b8', study_continuity_preserves_binding_force, conventional).
narrative_ontology:cs_reference_frame('74032b5e-dc66-4804-b2cd-e0f1dee150b8', textual_interpretation_as_sacrifice).
narrative_ontology:cs_drift_state('74032b5e-dc66-4804-b2cd-e0f1dee150b8', contemporary_jewish_communities, gap(stable, minor, true)).
narrative_ontology:cs_created_at('74032b5e-dc66-4804-b2cd-e0f1dee150b8', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__study_as_performance, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__study_as_performance, textual_scholars).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__study_as_performance, rabbinic_interpreters).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__study_as_performance, ritual_study_practitioners).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__study_as_performance, halakhic_continuity_doctrine).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__study_as_performance, hermeneutic_performativity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Engage deeply with sacrifice law texts as their primary professional and spiritual practice. This reading legitimizes their interpretive labor as direct fulfillment of the obligation, not as preparatory or commemorative work. Their scholarly engagement is the performance itself; the obligation flows through their study. They have access to texts, interpretive communities, and institutional support (yeshivas, academic departments, research programs).
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, textual_scholars, beneficiary,
    organized, generational, mobile, global).

% Authoritatively frame and enforce the interpretive reading that study fulfills the obligation. They determine which study counts, which interpretive methods are legitimate, what textual engagement satisfies the commandment. They set the doctrinal standards and adjudicate boundary cases. They collect authority and interpretive control over the constraint's meaning.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, rabbinic_interpreters, agenda_setter,
    institutional, generational, arbitrage, global).

% Participate in structured study of sacrifice law (in study circles, prayer services, communal learning). For them the reading transforms obligatory study into the performance of obligation itself. They are bound by communal expectations of participation but gain legitimate status as fulfilling the commandment through their study. Their access depends on local community institutions and knowledge of Hebrew/Aramaic.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, ritual_study_practitioners, beneficiary,
    moderate, biographical, constrained, local).

% Cannot participate in the textual study that this reading makes central to obligation fulfillment. They lack literacy in classical languages, access to texts and scholarly communities, or time required for sustained interpretive engagement. Under this reading, their ability to fulfill the commandment is mediated entirely through others' textual performance. This reading restructures who can be a full participant in the obligation.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, non_literate_community_members, excluded,
    powerless, biographical, trapped, local).

% Dispute that study alone fulfills the obligation; they hold that physical restoration and actual sacrifice are required. They are written out of the normative framing under this reading, though they remain organized in alternative communities and scholarly traditions. Their voices are present but treated as insufficiently grounded in the contemporary interpretive framework.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, performance_only_advocates, excluded,
    moderate, generational, constrained, global).

% Maintain that the obligation is suspended (not fulfilled) pending messianic redemption. They see this reading as premature closure, treating study as adequate when the true fulfillment awaits restoration. They hold alternative interpretive authority grounded in eschatological premises and resist full incorporation into the study-as-performance framework.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, messianic_advocates, excluded,
    moderate, civilizational, identity_locked, global).

% Treat the texts as cultural heritage to preserve and analyze without binding normative force. They study for historical understanding and preservation, not obligation fulfillment. This reading subordinates their interpretive mode to a binding-obligation reading, making their work instrumental to fulfilling commandment rather than freestanding scholarship.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, archival_preservationists, observer,
    moderate, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_continuity__study_as_performance, diffuse).
narrative_ontology:fixing_cost_class(sacrifice_obligation_continuity__study_as_performance, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains continuity of the sacrifice obligation across a historical period when physical performance is impossible (post-Temple destruction). Study and interpretation substitute for and transmit the obligation in a form that can persist through textual engagement, keeping the commandment alive in communal memory and legal consciousness. Coordinates interpretation across generations and communities by establishing study as the legitimate performance mode.
% TRANSFER_FUNCTION: Redistributes access to and authority over obligation fulfillment: textual scholars and rabbinic interpreters gain concentrated interpretive authority; non-literate community members lose direct access to fulfillment (which now requires literacy and study participation); scholars in rival readings lose legitimacy within the dominant interpretive framework.
% ABSENT_VOICES: Performance-only advocates and messianic suspension advocates are structurally excluded from the normative framing. They dispute the reading's core premise (that study substitutes for performance) but are not seated in the interpretive authority structure. Non-literate community members are also absent from the deliberative space that shapes what counts as fulfillment.
% DISAPPEARANCE_RATIONALE: If this reading disappeared and were replaced by a performance-only reading, the entire structure of obligation would reorganize: study would become preparatory rather than performative, textual scholarship would lose binding-obligation status, and the interpretive authority of textual scholars would shift from primary to secondary. If replaced by archival-preservation reading, study would continue but as historical preservation without commandment force, removing the obligation dimension entirely.
% FOUNDING_PROBLEM: After destruction of the Temple and end of physical sacrifice, how does the commandment to offer sacrifice persist as a binding obligation? How is the obligation kept alive, transmitted, and fulfilled when its original performance mode is historically impossible?
% FOUNDING_PROBLEM_CORROBORATION: The rabbinic tradition attests the problem is live and this solution is the binding answer (Talmudic statements that 'whoever engages in the study of sacrifice is considered as if they offered it,' Menachot 110a). Historians and textual scholars outside the rabbinic community corroborate that the problem statement (post-Temple obligation transmission) is historically accurate. However, performance-only and messianic advocates dispute whether this reading genuinely solves the problem or defers it.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__study_as_performance, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__study_as_performance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__study_as_performance, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(sacrifice_obligation_continuity__study_as_performance, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__study_as_performance_tests).
:- end_tests(sacrifice_obligation_continuity__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the reading does not require payment, coercive enforcement, or asymmetric resource transfer — study is the performance itself and is broadly accessible (texts are shared resources; interpretive communities are voluntary). Suppression is very low (0.12) because there is no coercive machinery maintaining the reading; alternative readings persist openly in scholarly and rabbinic discourse. Theater ratio is extremely low (0.08) — the performance is genuine textual engagement; there is no performative maintenance of a degraded function. Accessibility collapse is high (0.92) because once one accepts the reading's core premise (that study fulfills obligation), alternatives become logically incoherent within that framework — this is not suppression but axiological closure. Resistance is low (0.14) because the reading is maintained through ongoing scholarly consensus and textual authority, not against active resistance (though rival readings do exist). The measurement series shows slight erosion of extractiveness over 2000 years as the reading becomes consolidated and uncontested within mainstream rabbinic tradition — extractiveness tends toward the axis where no alternative is live and no compensation mechanism is needed.
 *
 * PERSPECTIVAL GAP:
 *   The rabbinic institutional seat and the textual scholar seat should align closely under this reading — both benefit from the study-as-performance framing, both drive the obligation through interpretive authority. Divergence appears at the excluded seats: performance-only advocates experience the reading as foreclosure of their alternative interpretation; messianic advocates experience it as premature closure; non-literate members experience it as exclusion from direct participation. The engine should compute Rope from the beneficiary seats (coordination, no asymmetric extraction) and partial Snare characteristics from the excluded seats (their reading is suppressed in the dominant framework, though not by coercive force). This reading's classification depends on which seat's directionality is adopted — a feature of kernel readings generally.
 *
 * DIRECTIONALITY LOGIC:
 *   The rabbinic agenda-setter seat has low directionality (near 0.0, beneficiary: derives institutional control and interpretive authority from the reading). Textual scholars have low directionality (0.1–0.2: benefit from legitimation of their work, gain access to the obligation's status). Ritual study practitioners have low-to-symmetric directionality (0.2–0.4: gain legitimacy from participation but also bear the obligation of sustained engagement). Non-literate members have high directionality (0.7–0.8: targeted by the reading's exclusion, their access is mediated through scholars). Performance-only advocates have high directionality (0.8: their interpretive reading is displaced, their authority is subordinated). No explicit override is needed — the structural data (beneficiary/victim/excluded declarations) drive the directionality derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mandatrophy-style capture by maintaining the obligation as genuinely binding (not degraded to ceremony or performance for performance's sake). The founding problem is live: the obligation must persist post-Temple, and this reading sustains it through textual engagement. Theater ratio stays extremely low because study is not performative maintenance of a degraded function — it is the function itself. The reading would become vulnerable to piton-drift if study were to become ritualized without genuine engagement (if scholars were reading mechanically to satisfy compliance rather than for interpretive understanding), but the measurement series shows stable low theater ratio across 2000 years, suggesting genuine engagement persists. The reading avoids the mandatrophy trap: it does not claim the obligation is obsolete (that would be archival preservation), it does not defer resolution (that would be messianic suspension), and it does not require performance it cannot sustain (that would trap the performance-only reading).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_engagement_authenticity,
    'Does genuine interpretive engagement (novel insights, sincere grappling with textual meaning) distinguish this reading''s performance from mechanized recitation or rote study? How much of the measured ''study'' is authentic engagement versus compliance performance?',
    'Longitudinal analysis of textual commentary production: rising novel interpretive output supports authentic engagement; stagnation or mere repetition of prior commentary suggests ritual performance without genuine study. Ethnographic observation of study practice (intentionality, depth of engagement) in contemporary communities.',
    'If engagement is predominantly mechanical compliance, theater_ratio should rise toward piton levels (0.4+), reclassifying the reading toward degraded constraint. If engagement remains authentic, the reading sustains its Rope classification. This omega governs whether the constraint is a live obligation or a performed obligation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_engagement_authenticity, empirical, 'Whether study is genuine interpretive engagement or ritualized performance.').

omega_variable(
    inclusion_boundary_contestation,
    'What counts as ''study'' for obligation fulfillment under this reading? Is textual engagement itself sufficient, or must it meet standards of rigor, intention, or community recognition? Who adjudicates the boundary between legitimate study and illegitimate engagement?',
    'Textual analysis of rabbinic rulings on study adequacy: if standards are strictly gatekept by authorities, the reading exhibits high extractiveness for excluded groups (rises toward 0.35+); if standards are permissively inclusive, extractiveness stays low. Examine whether non-scholars can meet the standard through accessible engagement or whether expertise is required.',
    'Narrow gatekeeping (high standards, expert-only fulfillment) reclassifies toward Tangled Rope — beneficiaries (scholars) are coordinated while non-scholars are excluded. Permissive inclusion (any textual engagement counts) maintains low extractiveness and Rope character. This omega tracks the reading''s tendency toward credential capture.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(inclusion_boundary_contestation, empirical, 'Whether the boundary of legitimate study is gatekept or permissive.').

omega_variable(
    interpretive_authority_concentration,
    'Is the reading''s maintenance dependent on rabbinic institutional authority, or does it derive legitimacy from independent textual reasoning? If authority is concentrated in institutional gatekeepers, to what extent does the reading function to extract interpretive control from lay practitioners?',
    'Historical analysis of dissent tolerance: if dissident interpretations of how to fulfill the obligation are treated as legitimate alternatives, authority is distributed; if they are suppressed or delegitimized, authority is concentrated. Examine whether lay-led study communities can generate new readings or whether novel interpretations must receive rabbinical endorsement.',
    'High authority concentration could elevate extractiveness as a mechanism for controlling obligatory practice (rises toward 0.35+) and would reclassify toward Tangled Rope with scholars/rabbinic authorities as beneficiaries and lay practitioners as coordinated-but-constrained. Distributed interpretive authority maintains low extractiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interpretive_authority_concentration, conceptual, 'Whether the reading''s legitimacy depends on concentrated institutional authority or distributed textual reasoning.').

omega_variable(
    kernel_reading_contest_status,
    'What is the contemporary status of the contest between the four readings of this kernel? Is one reading dominantly held as binding (closure), or do multiple readings coexist as live positions in rabbinic and Jewish communities?',
    'Institutional survey: document which reading is held as normatively binding in Haredi, Modern Orthodox, Conservative, and other communities. Examine whether alternative readings are treated as heretical, as permissible alternative opinion, or as equally authoritative.',
    'If this reading dominates across multiple communities, it is effectively foreclosing the siblings in those jurisdictions — the reading_relations should shift toward ''forecloses'' rather than ''coexists_with''. If alternatives remain live, ''coexists_with'' is correct. This omega routes the contestation into classification updates when empirical status changes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_status, empirical, 'Current status of the four readings in Jewish communities — dominant, contested, or coexistent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__study_as_performance, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(sacr_tr_t0, observed).
narrative_ontology:measurement(sacr_tr_t200, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 200, 0.1).
narrative_ontology:measurement_basis(sacr_tr_t200, observed).
narrative_ontology:measurement(sacr_tr_t600, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 600, 0.08).
narrative_ontology:measurement_basis(sacr_tr_t600, observed).
narrative_ontology:measurement(sacr_tr_t1000, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 1000, 0.07).
narrative_ontology:measurement_basis(sacr_tr_t1000, observed).
narrative_ontology:measurement(sacr_tr_t1400, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 1400, 0.07).
narrative_ontology:measurement_basis(sacr_tr_t1400, observed).
narrative_ontology:measurement(sacr_tr_t2000, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 2000, 0.08).
narrative_ontology:measurement_basis(sacr_tr_t2000, observed).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(sacr_be_t0, observed).
narrative_ontology:measurement(sacr_be_t200, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 200, 0.2).
narrative_ontology:measurement_basis(sacr_be_t200, observed).
narrative_ontology:measurement(sacr_be_t600, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 600, 0.19).
narrative_ontology:measurement_basis(sacr_be_t600, observed).
narrative_ontology:measurement(sacr_be_t1000, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 1000, 0.18).
narrative_ontology:measurement_basis(sacr_be_t1000, observed).
narrative_ontology:measurement(sacr_be_t1400, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 1400, 0.17).
narrative_ontology:measurement_basis(sacr_be_t1400, observed).
narrative_ontology:measurement(sacr_be_t2000, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 2000, 0.18).
narrative_ontology:measurement_basis(sacr_be_t2000, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_obligation_continuity__study_as_performance, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__study_as_performance, attachment_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_continuity__study_as_performance, 0.06).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__study_as_performance, sacrifice_obligation_continuity__performance_only).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__study_as_performance, sacrifice_obligation_continuity__messianic_suspension).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__study_as_performance, sacrifice_obligation_continuity__archival_preservation).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the contested kernel 'sacrifice_obligation_continuity'. All four readings share the same founding problem (how does sacrifice obligation persist post-Temple?) but propose different solutions instantiating different constraints with different ε values and beneficiary structures. Study-as-performance reading (this file): establishes study as the obligation's fulfillment; low extractiveness; textual scholars and rabbinic interpreters as beneficiaries. Performance-only reading: study is preparation only; obligation deferred until restoration; higher extractiveness for excluded performers. Messianic suspension: obligation held in eschatological suspension; study as maintenance of readiness. Archival preservation: obligation is obsolete; study is cultural preservation without normative force. These are separate constraints, not perspectives on one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
