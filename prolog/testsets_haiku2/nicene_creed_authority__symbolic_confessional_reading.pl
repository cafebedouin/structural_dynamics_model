% ============================================================================
% CONSTRAINT STORY: nicene_creed_authority__symbolic_confessional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_creed_authority__symbolic_confessional_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: nicene_creed_authority__symbolic_confessional_reading
 *   human_readable: Nicene Creed Authority: Symbolic Confessional Reading
 *   domain: systematic_theology/ecclesiology
 *
 * SUMMARY:
 *   The Nicene Creed stands at the intersection of ecclesiology, doctrine,
 *   and historical contingency. This constraint story models one reading of
 *   the creed's authority: it is understood as a historically contingent
 *   witness to apostolic faith emerging from specific political and
 *   theological circumstances, whose authority derives from community
 *   discernment and personal faith rather than from a centralized
 *   magisterium's enforcement. Under this reading, the creed functions as a
 *   boundary marker and identity confession, but not as a mandate for
 *   metaphysical uniformity. Local congregations coordinate on confessing the
 *   creed without requiring cognitive assent to every proposition or
 *   delegating interpretive authority to hierarchical structures. This
 *   reading has low extractiveness because it minimizes the rent-extraction
 *   potential of centralized doctrinal control; it has modest suppression
 *   because community discernment is the enforcement mechanism rather than
 *   institutional coercion; theater is low because the confessional function
 *   is genuine (the creed does coordinate identity) rather than performative.
 *
 * KEY AGENTS:
 *   - Local congregations: coordinate on shared confession without surrendering interpretive autonomy to centralized authority
 *   - Confessional communities: are the loci of authority, collectively discerning the creed's meaning and application
 *   - Individual believers: hold personal theological positions divergent from literal propositions while still confessing creedally
 *   - Centralized ecclesiastical authority: loses enforcement power over interpretation but retains canonical authority
 *   - Interfaith dialogue partners: become structurally enabled to engage without metaphysical conversion requirements
 *   - Historical scholarship: provides evidence for the creed's contingency and situates it within its historical context
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_creed_authority__symbolic_confessional_reading, 0.18).
domain_priors:suppression_score(nicene_creed_authority__symbolic_confessional_reading, 0.12).
domain_priors:theater_ratio(nicene_creed_authority__symbolic_confessional_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_creed_authority__symbolic_confessional_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(nicene_creed_authority__symbolic_confessional_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(nicene_creed_authority__symbolic_confessional_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_creed_authority__symbolic_confessional_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(nicene_creed_authority__symbolic_confessional_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_creed_authority__symbolic_confessional_reading, rope).
narrative_ontology:human_readable(nicene_creed_authority__symbolic_confessional_reading, "Nicene Creed Authority: Symbolic Confessional Reading").
narrative_ontology:topic_domain(nicene_creed_authority__symbolic_confessional_reading, "systematic_theology/ecclesiology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_creed_authority__symbolic_confessional_reading, '7ee60ef5-30a4-48e2-b9f5-37500019c26f').
narrative_ontology:cs_kernel_codification('7ee60ef5-30a4-48e2-b9f5-37500019c26f', fixed_text).
narrative_ontology:cs_authority_grounding('7ee60ef5-30a4-48e2-b9f5-37500019c26f', practice).
narrative_ontology:cs_interpretation_layer_present('7ee60ef5-30a4-48e2-b9f5-37500019c26f').
narrative_ontology:cs_reading_relation('7ee60ef5-30a4-48e2-b9f5-37500019c26f', nicene_creed_authority__strict_orthodox_reading, coexists_with).
narrative_ontology:cs_reading_relation('7ee60ef5-30a4-48e2-b9f5-37500019c26f', nicene_creed_authority__liturgical_habituation_reading, influences).
narrative_ontology:cs_axiom('7ee60ef5-30a4-48e2-b9f5-37500019c26f', foundational, authority_from_community_discernment).
narrative_ontology:cs_axiom_status(authority_from_community_discernment, holdable).
narrative_ontology:cs_axiom_grounding('7ee60ef5-30a4-48e2-b9f5-37500019c26f', authority_from_community_discernment, conventional).
narrative_ontology:cs_axiom('7ee60ef5-30a4-48e2-b9f5-37500019c26f', foundational, creed_as_historical_contingent_witness).
narrative_ontology:cs_axiom_status(creed_as_historical_contingent_witness, holdable).
narrative_ontology:cs_axiom_grounding('7ee60ef5-30a4-48e2-b9f5-37500019c26f', creed_as_historical_contingent_witness, empirically_contingent).
narrative_ontology:cs_reference_frame('7ee60ef5-30a4-48e2-b9f5-37500019c26f', post_enlightenment_historical_consciousness).
narrative_ontology:cs_drift_state('7ee60ef5-30a4-48e2-b9f5-37500019c26f', contemporary_ecumenical_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('7ee60ef5-30a4-48e2-b9f5-37500019c26f', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(nicene_creed_authority__symbolic_confessional_reading, nicene_creed_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_creed_authority__symbolic_confessional_reading, local_congregations).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__symbolic_confessional_reading, confessional_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__symbolic_confessional_reading, individual_believers).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__symbolic_confessional_reading, interfaith_dialogue_partners).
narrative_ontology:constraint_victim(nicene_creed_authority__symbolic_confessional_reading, centralized_ecclesiastical_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Use the creed as a shared confessional witness and identity marker without requiring metaphysical uniformity or cognitive assent to every proposition. They interpret the creed's language as historically situated testimony rather than ontological mandate, permitting local theological pluralism and interfaith dialogue. They benefit from inherited tradition that legitimates their practice without enforcing doctrinal conformity.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__symbolic_confessional_reading, local_congregations, beneficiary,
    organized, generational, mobile, local).

% Under this reading, centralized authorities lose the power to enforce metaphysical orthodoxy as a condition of belonging. Their authority derives from the creed's canonical status, but that status cannot be leveraged to suppress theological dissent or exclude communities that read the creed symbolically rather than literally. Their exit from the interpretive framework would require abandoning the creed's authority altogether, but their institutional power rests substantially on that authority.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__symbolic_confessional_reading, centralized_ecclesiastical_authority, payer,
    institutional, civilizational, trapped, global).

% Operate as the loci of authority: each community's discernment of the creed's meaning and application becomes constitutive of the creed's authority. They jointly coordinate on the creed's canonical status without surrendering interpretive autonomy to a centralized arbiter. Benefit from shared confession without requiring cognitive identity of doctrine.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__symbolic_confessional_reading, confessional_communities, beneficiary,
    organized, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(nicene_creed_authority__symbolic_confessional_reading, confessional_communities, agenda_setter).

% May hold personal theological positions that diverge from the creed's literal propositions while still confessing the creed liturgically and doctrinally. Personal faith and community confession are decoupled; belonging does not require metaphysical assent to every article. Benefit from identity continuity with the tradition while maintaining interpretive freedom.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__symbolic_confessional_reading, individual_believers, beneficiary,
    moderate, biographical, constrained, local).

% When the creed is read symbolically rather than as metaphysical mandate, interfaith engagement becomes structurally enabled. Other traditions can recognize the creed as Christian identity witness without requiring metaphysical conversion. This reading lowers the barrier to cross-tradition learning and cooperation.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__symbolic_confessional_reading, interfaith_dialogue_partners, beneficiary,
    moderate, generational, mobile, global).

% Traditions that reject the creed altogether (Arian groups, non-Trinitarian communities) are not made welcome by this reading; the creed retains normative force as canonical confession. But they are not classified as heretics requiring sanction under this framework — they are simply outside the communion of creedal confession. Their exclusion is less punitive than under the strict_orthodox_reading.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__symbolic_confessional_reading, doctrinal_heterodoxy, excluded,
    moderate, generational, mobile, global).

% Investigates the creed's historical contingency and evolution. This reading aligns with scholarly consensus that the creed emerged from specific historical and political contexts, not from revelation independent of history. Scholarship becomes a tool for understanding the creed rather than a threat to its authority.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__symbolic_confessional_reading, historical_scholarship, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nicene_creed_authority__symbolic_confessional_reading, diffuse).
narrative_ontology:fixing_cost_class(nicene_creed_authority__symbolic_confessional_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared confessional witness recognizable across denominational and temporal boundaries: congregations coordinate on the affirmation 'we confess the Nicene faith' without requiring agreement on metaphysical interpretation, cognitive assent to every article, or conformity to a single institutional arbiter.
% TRANSFER_FUNCTION: Transfers authority from centralized ecclesiastical hierarchies to local communities and individual believers' discernment. Under this reading, authority flows upward from confessional communities rather than downward from institutional magisteria. The cost to centralized authority is the loss of enforcement power over interpretation; the benefit to congregations is autonomy within canonical tradition.
% ABSENT_VOICES: Strict orthodox defenders within institutional churches who see this reading as corrosive to doctrinal unity would object if present; they argue that community discernment and individual faith-reading open the door to relativism. Doctrinal minorities and dissenters within the tradition who would prefer the creed to be abandoned altogether are also outside this framework — the reading presupposes the creed's canonical status even while loosening its interpretive strictness.
% DISAPPEARANCE_RATIONALE: If this reading disappeared — if the symbolic confessional authority structure were replaced by strict enforcement orthodoxy — the landscape shifts: local congregations would lose interpretive autonomy, interfaith dialogue would become theologically fraught, and institutional ecclesiastical power would recentralize. Conversely, if the creed itself were abandoned, the coordination function it provides would need to be replaced by a different shared confession or by purely local identity markers.
% FOUNDING_PROBLEM: The creed was born from the Council of Nicaea (325 CE) as a response to Arian controversy: how to affirm the apostolic faith against a heretical reading while maintaining communion across diverse local churches with distinct theological vocabularies and concerns. The creed was meant to be a boundary marker against false doctrine, not a mandate for metaphysical identity across all believers.
% FOUNDING_PROBLEM_CORROBORATION: Historical-critical scholarship (Hanson, Behr, Gregg and Groh) documents the creed's political and contingent origins; contemporary ecumenical councils and interfaith commissions attest that the creed can function as a shared witness without requiring metaphysical uniformity; magisterial church authorities (Vatican II, World Council of Churches statements) recognize plural interpretations. Outside corroboration comes from liturgical theologians and systematic theologians who argue for symbolic rather than metaphysical readings (e.g., George Lindbeck's cultural-linguistic account). Institutional authorities defending strict orthodoxy provide no independent corroboration — they are the named beneficiary of the competing reading.
narrative_ontology:disappearance_verdict(nicene_creed_authority__symbolic_confessional_reading, world_rearranges).
narrative_ontology:founding_problem_status(nicene_creed_authority__symbolic_confessional_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_creed_authority__symbolic_confessional_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nicene_creed_authority__symbolic_confessional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nicene_creed_authority__symbolic_confessional_reading, 0.18, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_creed_authority__symbolic_confessional_reading_tests).
:- end_tests(nicene_creed_authority__symbolic_confessional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18 at interval end) because the reading minimizes the opportunity for centralized institutional extraction: authority is diffuse, interpretive pluralism is permitted, and the creed's authority rests on community assent rather than hierarchical decree. Suppression is also low (0.12) because the constraint operates through coordination and shared meaning-making rather than through coercive enforcement or threat. Theater ratio is minimal (0.08) because the confessional function is genuine — congregations actually coordinate on the creed and actually use it as a boundary marker and identity affirmation. The slight downward drift in all metrics across the interval reflects the increasing scholarly consensus and ecumenical acceptance of this reading over the past 50–70 years, reducing the need for active enforcement as the interpretive framework becomes more normalized. The measurement series is authored on a single shared time grid across all three tracked metrics.
 *
 * PERSPECTIVAL GAP:
 *   The two institutional seats — local congregations and centralized ecclesiastical authority — experience radically different constraint architectures. For congregations, the creed is rope: it coordinates identity, provides canonical legitimacy, and permits interpretive freedom. For centralized authority, the constraint looks like snare from their seat: they lose coercive power, their enforcement machinery becomes illegitimate, and they cannot exit without abandoning their institutional raison d'être. The engine computes this per-seat divergence from power (organized vs. institutional), exit options (mobile vs. identity_locked), and beneficiary/victim declarations. The authored claim (rope) matches the congregation seat; the metrics capture the structural asymmetry that makes it snare-like from the authority seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Local congregations benefit from shared canonical confession without losing interpretive autonomy — they are beneficiaries (d near 0.0). Individual believers benefit from identity continuity with tradition while maintaining personal theological freedom — they are beneficiaries (d near 0.0). Confessional communities sit near both beneficiary and agenda-setter: they collectively enact the creed's meaning, so d is close to 0.0 (they set the agenda). Centralized ecclesiastical authority is the structural target under this reading: they lose the power to enforce metaphysical orthodoxy and cannot sanction divergent interpretations. Their d is near 1.0 (the constraint extracts enforcement power from them). Interfaith dialogue partners have d near 0.0 (they benefit from lowered barriers to engagement). This inversion of the typical authority topology — where centralized authority typically benefits from doctrinal enforcement — is the distinguishing feature of this reading. The exit options for each agent modulate the directionality: congregations have mobile exit (they could align with strict orthodoxy if they chose), so d is not pinned at the beneficiary extreme; centralized authority is identity_locked to the magisterial role (they cannot exit without abandoning institutional existence), so d is higher than exit options alone would determine.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to affirm apostolic faith against heretical deviation while maintaining communion) remains contested. Under this reading, the problem persists but its solution is reframed: communion is maintained through shared confession and community discernment rather than through enforcement of metaphysical uniformity. The creed's founding function has not become obsolete; its execution has been redistributed from centralized to local authority. This is not mandatrophy in the sense of atrophied function — the confessional, boundary-marking, communion-affirming functions are still active. But from the perspective of centralized ecclesiastical authority, it could appear as mandatrophy: the creed was built to enforce doctrinal unity, but that founding mandate is no longer being executed (or is being executed by a different agent with different methods). The omega variables below capture this contested genealogy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    community_discernment_ambiguity,
    'What constitutes ''community discernment'' of the creed''s meaning? Is it the consensus of ordained leadership, the lived faith of ordinary believers, scholarly-historical consensus, or some combination?',
    'Comparative study of how different confessional communities actually practice interpretive authority; ethnographic observation of creed-interpretation in liturgical, educational, and doctrinal settings; analysis of how dissent is handled within communities.',
    'If community discernment privileges ordained leadership, the reading collapses toward institutional authority (extractiveness rises). If it privileges ordinary believers, the reading strengthens local autonomy (extractiveness remains low). If it requires scholarly consensus, the reading becomes dependent on academic institutions (new agent structure).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(community_discernment_ambiguity, conceptual, 'Structural ambiguity in what counts as legitimate community interpretation of the creed.').

omega_variable(
    reading_vs_strict_orthodoxy_foreclosure,
    'Does the symbolic_confessional_reading genuinely coexist with the strict_orthodox_reading within a single framework (e.g., a church that permits both readings among its members) or do the readings foreclose each other?',
    'Empirical survey of actual church practice: do denominations that officially adopt this reading also permit members to hold strict-orthodox interpretations? Do official statements forbid the competing interpretation?',
    'If the readings coexist, the engine relation is coexists_with (contested kingdom). If mutual exclusion is enforced, the relation is forecloses or is-foreclosed-by (depends on institutional power). This affects the network topology of the kernel constraint family.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_strict_orthodoxy_foreclosure, empirical, 'Whether the symbolic_confessional_reading permits strict-orthodox interpretation as an alternative within the same community, or if adoption of this reading requires rejection of strict orthodoxy.').

omega_variable(
    magisterial_authority_vs_community_discernment,
    'When centralized ecclesiastical authority and community discernment reach different conclusions about the creed''s meaning, which yields? Is there a tiebreaker, or is the divergence itself a permanent feature of this reading?',
    'Case analysis from church history (Reformation debates, Vatican II, contemporary ecumenical discussions): empirical documentation of how divergences were resolved or left unresolved. Structural analysis of whether the reading commits to a resolution mechanism or leaves it indeterminate.',
    'If magisterium yields to communities, extractiveness remains low and the reading is stable. If community discernment yields to magisterium, extractiveness rises and the reading collapses toward strict orthodoxy. If divergence is unresolved, the constraint may be categorized as contested rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(magisterial_authority_vs_community_discernment, empirical, 'Authority hierarchy when institutional magisterium and community discernment conflict on creed interpretation.').

omega_variable(
    personal_faith_vs_communal_confession,
    'When individual faith-reading diverges from communal confessional practice, which takes precedence in determining whether an individual belongs to the communion? Must personal belief align with shared confession, or is the sharing of confession sufficient for belonging without belief-alignment?',
    'Empirical examination of actual church discipline and belonging criteria: are people excluded for heterodox belief if they still confess the creed? Are people included despite belief-heterodoxy if they confess?',
    'If confession suffices for belonging despite belief-divergence, extractiveness stays low and suppression is minimal (personal faith is protected). If belief-alignment is required, extractiveness rises and suppression increases (individuals must conform internally, not just externally).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(personal_faith_vs_communal_confession, empirical, 'Structural relationship between individual personal faith and communal confessional practice in determining belonging.').

omega_variable(
    kernel_reading_bifurcation,
    'Is the symbolic_confessional_reading one stable reading, or does it bifurcate into two distinct constraints depending on whether the locus of authority is understood as (a) local congregations coordinating with each other or (b) individual believers'' personal discernment?',
    'Structural analysis: do the two loci generate different ε values, different beneficiary/victim sets, different enforcement mechanisms? If yes, decompose into separate constraint stories per the ε-invariance principle. If no, they are genuinely one reading.',
    'Bifurcation would require two separate JSON files (local_congregational_authority and personal_faith_authority), linked via network.affects_constraints. Single-file treatment is valid only if ε and agent structure are stable across both loci.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_bifurcation, conceptual, 'Whether the symbolic_confessional_reading''s indeterminacy between congregational and personal loci of authority constitutes a single constraint or two.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_creed_authority__symbolic_confessional_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nice_tr_t0, nicene_creed_authority__symbolic_confessional_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(nice_tr_t0, observed).
narrative_ontology:measurement(nice_tr_t5, nicene_creed_authority__symbolic_confessional_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement_basis(nice_tr_t5, observed).
narrative_ontology:measurement(nice_tr_t10, nicene_creed_authority__symbolic_confessional_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement_basis(nice_tr_t10, observed).
narrative_ontology:measurement(nice_tr_t15, nicene_creed_authority__symbolic_confessional_reading, theater_ratio, 15, 0.08).
narrative_ontology:measurement_basis(nice_tr_t15, observed).
narrative_ontology:measurement(nice_tr_t20, nicene_creed_authority__symbolic_confessional_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement_basis(nice_tr_t20, observed).
narrative_ontology:measurement(nice_tr_t25, nicene_creed_authority__symbolic_confessional_reading, theater_ratio, 25, 0.08).
narrative_ontology:measurement_basis(nice_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(nice_be_t0, nicene_creed_authority__symbolic_confessional_reading, base_extractiveness, 0, 0.24).
narrative_ontology:measurement_basis(nice_be_t0, observed).
narrative_ontology:measurement(nice_be_t5, nicene_creed_authority__symbolic_confessional_reading, base_extractiveness, 5, 0.22).
narrative_ontology:measurement_basis(nice_be_t5, observed).
narrative_ontology:measurement(nice_be_t10, nicene_creed_authority__symbolic_confessional_reading, base_extractiveness, 10, 0.2).
narrative_ontology:measurement_basis(nice_be_t10, observed).
narrative_ontology:measurement(nice_be_t15, nicene_creed_authority__symbolic_confessional_reading, base_extractiveness, 15, 0.19).
narrative_ontology:measurement_basis(nice_be_t15, observed).
narrative_ontology:measurement(nice_be_t20, nicene_creed_authority__symbolic_confessional_reading, base_extractiveness, 20, 0.18).
narrative_ontology:measurement_basis(nice_be_t20, observed).
narrative_ontology:measurement(nice_be_t25, nicene_creed_authority__symbolic_confessional_reading, base_extractiveness, 25, 0.18).
narrative_ontology:measurement_basis(nice_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(nice_su_t0, nicene_creed_authority__symbolic_confessional_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement_basis(nice_su_t0, observed).
narrative_ontology:measurement(nice_su_t5, nicene_creed_authority__symbolic_confessional_reading, suppression_requirement, 5, 0.15).
narrative_ontology:measurement_basis(nice_su_t5, observed).
narrative_ontology:measurement(nice_su_t10, nicene_creed_authority__symbolic_confessional_reading, suppression_requirement, 10, 0.13).
narrative_ontology:measurement_basis(nice_su_t10, observed).
narrative_ontology:measurement(nice_su_t15, nicene_creed_authority__symbolic_confessional_reading, suppression_requirement, 15, 0.12).
narrative_ontology:measurement_basis(nice_su_t15, observed).
narrative_ontology:measurement(nice_su_t20, nicene_creed_authority__symbolic_confessional_reading, suppression_requirement, 20, 0.12).
narrative_ontology:measurement_basis(nice_su_t20, observed).
narrative_ontology:measurement(nice_su_t25, nicene_creed_authority__symbolic_confessional_reading, suppression_requirement, 25, 0.12).
narrative_ontology:measurement_basis(nice_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_creed_authority__symbolic_confessional_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(nicene_creed_authority__symbolic_confessional_reading, 0.12).
narrative_ontology:affects_constraint(nicene_creed_authority__symbolic_confessional_reading, nicene_creed_authority__strict_orthodox_reading).
narrative_ontology:affects_constraint(nicene_creed_authority__symbolic_confessional_reading, nicene_creed_authority__liturgical_habituation_reading).

% DUAL FORMULATION NOTE:
% The nicene_creed_authority kernel decomposes into three readings with structurally distinct authority topologies, beneficiary sets, and extractiveness profiles. The symbolic_confessional_reading (this constraint) inverts the typical institutional authority structure by locating authority in community discernment and personal faith rather than in centralized magisterium. The strict_orthodox_reading maintains centralized enforcement power (higher ε, higher suppression). The liturgical_habituation_reading sidesteps the authority question by making performance rather than cognition the binding mechanism. Each reading is a separate constraint story with its own ε-invariant claim and metrics; they are linked here as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nicene_creed_authority__symbolic_confessional_reading, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
