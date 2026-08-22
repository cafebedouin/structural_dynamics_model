% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__rupture_traditionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_doctrinal_authority__rupture_traditionalist_reading, []).

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
 *   constraint_id: vatican_ii_doctrinal_authority__rupture_traditionalist_reading
 *   human_readable: Vatican II Doctrinal Authority: Rupture and Ambiguity (Traditionalist Reading)
 *   domain: ecclesiology/institutional history/hermeneutics
 *
 * SUMMARY:
 *   Vatican II (1962–1965) was presented to the universal Church as a
 *   pastoral council that would renew the Church without changing doctrine.
 *   This reading rejects that self-description and asserts instead that
 *   Vatican II represents a rupture with pre-conciliar doctrine: its
 *   documents were deliberately crafted through compromise to absorb
 *   competing theological visions (ressourcement, nouvelle théologie,
 *   progressive ecclesiology) into ambiguous formulations that appear
 *   continuous with tradition while actually enabling radical
 *   reinterpretation. The constraint operates through this ambiguity: the
 *   post-conciliar implementation (1965–1985) read the Council's permissive
 *   silences as mandates for transformation, while the Council fathers and
 *   even Pope Paul VI understood themselves to be reforming within
 *   continuity. The traditionalist reading holds that the documents contain
 *   errors and internal contradictions that predictably enabled heterodoxy;
 *   the constraint's persistence depends on suppressing the pre-conciliar
 *   theological and liturgical alternatives and framing doctrinal clarity
 *   itself as neo-scholastic rigidity requiring deconstruction.
 *
 * KEY AGENTS:
 *   - Vatican II Council Fathers — institutional agenda-setters; voted conciliar documents into being through compromise processes
 *   - Progressive Reformist Faction — powerful beneficiaries; gained legitimacy for previously marginalized positions through ambiguous texts
 *   - Episcopal Modernizers — institutional beneficiaries and secondary agenda-setters; implemented Council permissively through episcopal conferences
 *   - Traditional Liturgy Communities — moderate-power victims; identity-locked; experienced suppression of constitutive liturgical practice
 *   - Doctrinal Clarity Defenders — moderate-power victims; constrained; forced to navigate competing authorized interpretations
 *   - Missionary Zeal Carriers — powerless victims; trapped; experienced dissolution of missionary rationale through conciliar relativization
 *   - Vatican Curia — institutional victims; constrained; experienced displacement of enforcement capacity over doctrine
 *   - Continuity Doctrine Defenders — excluded; voice structurally suppressed in post-conciliar institutions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.82).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.68).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, "Vatican II Doctrinal Authority: Rupture and Ambiguity (Traditionalist Reading)").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, "ecclesiology/institutional history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__rupture_traditionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 'dde8ec94-d016-4b84-91eb-3f82c896a0b8').
narrative_ontology:cs_kernel_codification('dde8ec94-d016-4b84-91eb-3f82c896a0b8', formalized).
narrative_ontology:cs_authority_grounding('dde8ec94-d016-4b84-91eb-3f82c896a0b8', lineage).
narrative_ontology:cs_interpretation_layer_present('dde8ec94-d016-4b84-91eb-3f82c896a0b8').
narrative_ontology:cs_reading_relation('dde8ec94-d016-4b84-91eb-3f82c896a0b8', vatican_ii_doctrinal_authority__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('dde8ec94-d016-4b84-91eb-3f82c896a0b8', vatican_ii_doctrinal_authority__rupture_progressive_reading, coexists_with).
narrative_ontology:cs_reading_relation('dde8ec94-d016-4b84-91eb-3f82c896a0b8', vatican_ii_doctrinal_authority__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('dde8ec94-d016-4b84-91eb-3f82c896a0b8', foundational, conciliar_texts_contain_doctrinal_rupture).
narrative_ontology:cs_axiom_status(conciliar_texts_contain_doctrinal_rupture, holdable).
narrative_ontology:cs_axiom_grounding('dde8ec94-d016-4b84-91eb-3f82c896a0b8', conciliar_texts_contain_doctrinal_rupture, empirically_contingent).
narrative_ontology:cs_axiom('dde8ec94-d016-4b84-91eb-3f82c896a0b8', foundational, ambiguity_enables_heterodox_implementation).
narrative_ontology:cs_axiom_status(ambiguity_enables_heterodox_implementation, holdable).
narrative_ontology:cs_axiom_grounding('dde8ec94-d016-4b84-91eb-3f82c896a0b8', ambiguity_enables_heterodox_implementation, empirically_contingent).
narrative_ontology:cs_axiom('dde8ec94-d016-4b84-91eb-3f82c896a0b8', secondary, doctrinal_continuity_is_ecclesial_good).
narrative_ontology:cs_axiom_status(doctrinal_continuity_is_ecclesial_good, holdable).
narrative_ontology:cs_axiom_grounding('dde8ec94-d016-4b84-91eb-3f82c896a0b8', doctrinal_continuity_is_ecclesial_good, deontological).
narrative_ontology:cs_reference_frame('dde8ec94-d016-4b84-91eb-3f82c896a0b8', pre_conciliar_doctrinal_clarity_and_liturgical_stability).
narrative_ontology:cs_drift_state('dde8ec94-d016-4b84-91eb-3f82c896a0b8', post_vatican_ii_implementation_1985, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('dde8ec94-d016-4b84-91eb-3f82c896a0b8', '2026-06-12T09:15:00Z').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, progressive_reformist_faction).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, episcopal_modernizers).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditional_liturgy_communities).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, doctrinal_clarity_defenders).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, missionary_zeal_carriers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_curia).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convened by Pope John XXIII and later Paul VI; authored the conciliar documents through committee process that built consensus by absorbing competing theological traditions into ambiguous formulations. Acted as the primary decision-making body establishing the new framework; their votes bound the universal Church.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_council_fathers, agenda_setter,
    institutional, generational, analytical, universal).

% Gained institutional legitimacy and doctrinal opening for positions previously marginalized: ressourcement theology, ecumenism, engagement with modernity, liturgical experimentation. The textual ambiguities permit expansive interpretation aligned with their reform agenda; post-conciliar implementation accelerates and extends conciliar directives beyond textual bounds. They interpret the Council's 'spirit' as authorizing ongoing transformation.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, progressive_reformist_faction, beneficiary,
    powerful, generational, mobile, global).

% National episcopal conferences gained expanded authority post-Council; modernizers used this authority to implement reforms in diocesan practice, seminary education, and liturgy, reading the conciliar ambiguities permissively. They become secondary agenda-setters through their implementation choices, reinforcing reformist readings into institutional practice.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, episcopal_modernizers, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, episcopal_modernizers, agenda_setter).

% Experience the constraint as the suppression and eventual prohibition of the Tridentine Mass, the liturgical framework constitutive of their religious identity for centuries. The Council's ambiguous language on 'reform' of the liturgy was read by post-conciliar authorities as mandate for wholesale replacement, not modification. Exit (returning to pre-conciliar practice) now means formal schism or marginalization within the institutional Church.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditional_liturgy_communities, payer,
    moderate, biographical, identity_locked, local).

% Seminarians, theologians, and clergy who require clear doctrinal formulation for teaching and pastoral practice. They bear the cost of ambiguous conciliar texts that refuse to settle contested questions (divine mercy vs. justice, relation of faith to works, proper scope of episcopal authority). Forced to navigate competing authorized interpretations, they experience doctrinal clarity as actively suppressed in favor of 'development' and 'dialogue.'
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, doctrinal_clarity_defenders, payer,
    moderate, generational, constrained, global).

% Missionary orders and pastoral agents committed to evangelization face the constraint as dissolution of missionary motivation: the Council's language on other religions, on the possibility of salvation outside the Church, and on inculturation as permission for doctrinal relativization undermines the urgency of conversion and the coherence of missionary theology. They are trapped because departure from missionary orders means loss of community, financial security, and legitimacy; staying means practicing mission under delegitimized rationales.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, missionary_zeal_carriers, payer,
    powerless, biographical, trapped, global).

% Traditionally the keeper of doctrinal continuity and custodian of pre-conciliar discipline. Post-Council, experiences the constraint as partial displacement of authority: the papal magisterium is now mediated through episcopal conferences and local interpretation; doctrinal enforcement became difficult when the conciliar texts themselves contained the ambiguities being weaponized for innovation. They bear the cost of eroded enforcement capacity.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_curia, payer,
    institutional, generational, constrained, universal).

% Steward of the Council's implementation; faced with the rapidly multiplying heterodox interpretations of ambiguous conciliar texts. By his later writings (Mysterium Fidei, Evangelii Nuntiandi) attempted to constrain the 'spirit of the Council' back toward continuity claims, but lacked textual leverage — the ambiguities had become institutional facts.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, pope_paul_vi, observer,
    institutional, biographical, analytical, universal).

% Theologians and bishops who insisted the Council taught organic development and continuity (the continuity_reading) are structurally excluded from the beneficiary coalition because their reading requires the conciliar texts to be unambiguous and continuous with prior doctrine — a reading suppressed by the post-conciliar institutional drift toward accepting discontinuity. Their voice would challenge the premise that ambiguity and error explain post-conciliar developments.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, continuity_doctrine_defenders, excluded,
    moderate, generational, identity_locked, global).

% Analytical seat: historians like John O'Malley and Giuseppe Alberigo who examine the Council's actual deliberations, textual evolution, and political dynamics reveal the extent to which ambiguities were deliberate compromises, not errors — yet this historical record is institutionally kept at arm's length from magisterial application.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_council_historians, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, progressive_reformist_faction).
narrative_ontology:fixing_cost_class(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Council coordinated competing theological factions within the Church through texts that absorbed their claims into layered or ambiguous formulations, enabling consensus without requiring actual doctrinal settlement. This coordination function was achieved by not resolving contradictions (on divine mercy vs. divine justice, on nature of episcopal authority, on salvation outside the Church).
% TRANSFER_FUNCTION: The arrangement transfers doctrinal authority from clearly defined magisterial formulations to interpretive frameworks that privilege 'the spirit of the Council' over textual limits. It transfers from the pre-conciliar magisterium to the post-conciliar episcopal conferences and their reformist theologians. It transfers from institutional enforcement of doctrinal boundaries to permission structures for innovation framed as 'development.'
% ABSENT_VOICES: Traditionalist bishops and theologians who objected to the Council's direction (Cardinals Ottaviani, Ruffini; Archbishop Lefebvre) were structurally marginalized during and after deliberations. They would object that the conciliar texts represent a rupture with prior doctrine and that the ambiguities enable precisely the heterodoxy they warned against. They are excluded from the post-conciliar authority structures interpreting the Council.
% DISAPPEARANCE_RATIONALE: If the constraint (the ambiguous texts, their permissive interpretation, and the suppression of pre-conciliar alternatives) vanished overnight, the Church would face stark choices: either return to pre-conciliar discipline and doctrine, or formally settle what the Council left ambiguous. Either path would reorganize the entire institutional landscape of post-conciliar Catholicism. The progressive reformist faction would lose their textual leverage; traditionalist communities would recover legitimate liturgical space; the episcopal conferences would lose the ambiguity they deploy to implement local reforms.
% FOUNDING_PROBLEM: The pre-conciliar Church was perceived by reformers as juridically rigid, pastorally disconnected from modern questions, defensively hostile to other religions and secular modernity, and burdened by excessive centralization of authority in the Roman Curia. Vatican II was convoked to address these perceptions through pastoral renewal and aggiornamento (updating).
% FOUNDING_PROBLEM_CORROBORATION: Progressive Council fathers (Cardinal König, Cardinal Suenens) and subsequent theological interpreters attest the founding problem was real and required Council action. Traditionalist observers (Archbishop Lefebvre, Cardinal Ottaviani, contemporary theologian Aidan Nichols) attest that the founding problem was exaggerated and that the Council's ambiguous response enabled solutions more radical than the actual problems warranted. Historians of the Council (O'Malley, Alberigo, Routhier) document both the genuine pastoral concerns AND the deliberate textual compromises that enabled radical post-conciliar reinterpretations. The corroboration is mixed — no seat outside the benefiting parties endorses the reformist reading uncritically.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82 at interval end) is high because the constraint transfers doctrinal authority from pre-conciliar clarity to post-conciliar interpretive permission structures that systematically favor reformist readings. The trajectory rises from 0.35 (1962, pre-conciliar institutional facts still in force) to 0.82 (1985, the reformist reading has become institutionally dominant after two decades of implementation). Suppression (0.68) is high because the constraint's persistence requires active suppression of the pre-conciliar liturgy (prohibited in 1969), active marginalization of traditionalist voices from post-conciliar authority structures, and active reframing of doctrinal clarity as a defect rather than a virtue. The coercion grid shows asymmetry: suppression and accessibility collapse rise most steeply at the individual and class levels (traditional communities, ordinary believers unable to recover pre-conciliar practice), while organizational and structural suppression grow more gradually (bishops initially retain formal authority; the Curia's displacement is cushioned by the Council's ambiguities permitting multiple interpretations). Theater ratio (0.44 at interval end) reflects the performative maintenance of continuity claims: the Council is continually invoked as a bridge between tradition and modernism, yet the texts themselves become increasingly inert as real authority migrates to post-conciliar institutional practice. The measurement series tracks the post-Council drift as ambiguities are institutionalized into reformist policy and counter-resistance (traditionalist ordinations, underground Tridentine communities) is suppressed.
 *
 * PERSPECTIVAL GAP:
 *   This reading's structural divergence from the continuity_reading is encoded in the seated powers and exit options: from the progressives' seat, the Council represents liberation from rigid pre-conciliar constraints and permission for authentic development (low d, beneficiary end). From the traditionalists' seat, the Council represents enforced rupture from authentic tradition and suppression of continuity (high d, victim end). The engine computes these divergences from stakeholder power atoms and exit modality: progressive reformists (powerful, mobile) experience low effective extraction; traditional communities (moderate power, identity-locked) experience high effective extraction. A traditionalist-reading observer (analytical seat) notes that both readings invoke the same texts but derive opposite conclusions about continuity vs. rupture; the difference lies in how the texts' internal tensions are resolved — the reformist reading privileges the direction of innovation over the letter of continuity claims, while the traditionalist reading privileges doctrinal continuity assertions while attributing the practical rupture to errors and ambiguities.
 *
 * DIRECTIONALITY LOGIC:
 *   Progressive reformist faction (institutional, mobile exit): d ≈ 0.15 → beneficiary/subsidized. They gained institutional and doctrinal permission; their exit options improved (they can pursue reform now legitimated by Council authority). Episcopal modernizers (institutional, constrained): d ≈ 0.25 → mixed. They gained authority (episcopal conferences, diocesan discretion) but are constrained by the ambiguous texts they must still cite. Traditional liturgy communities (moderate, identity-locked): d ≈ 0.88 → victim/target. They lost their constitutive practice; exit means schism (Archbishop Lefebvre's path) or underground operation (SSPX); staying in the institutional Church means accepting suppression. Doctrinal clarity defenders (moderate, constrained): d ≈ 0.82 → victim/target. They experience doctrinal constraints as suppressed; clarity becomes something they must defend against institutional pressure to embrace ambiguity as development. Missionary carriers (powerless, trapped): d ≈ 0.91 → victim/target. Trapped by vows and institutional dependency; missionary zeal (the felt urgency of conversion, the coherence of missionary theology) is actively delegitimized. Vatican Curia (institutional, constrained): d ≈ 0.65 → partial victim. They retain formal authority but experience erosion of enforcement capacity; their traditional role as doctrinal custodian is displaced by the Council's claim that development (not preservation) is the norm. Continuity doctrine defenders (moderate, identity-locked): d ≈ 0.86 → victim/target; excluded from power structures so structural derivation underestimates their victimhood. No directionality overrides needed: the structural derivation tracks the actual power distribution and exit asymmetries accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint presents the classical mandatrophy signature in this reading: Vatican II was founded to solve a genuine pastoral problem (pre-conciliar rigidity, institutional disconnection from modernity, defensive posture toward other faiths). This founding problem was live in 1962. By 1985, the founding problem status is contested: progressives argue it remains live and requires ongoing reform; traditionalists argue it is dead (post-conciliar liberalization and doctrinal confusion have replaced rigidity with dissolution). The disappearance verdict (world_rearranges) combined with founding_problem_status=contested signals mandatrophy: the institutional arrangements created to address a specific pastoral problem have persisted past the point where any party outside the beneficiary coalition believes the problem is live. The constraint now extracts from those who held the founding problem to be essential (doctrinal defenders, traditionalists, missionary orders) by requiring them to internalize a narrative of progress and development even as the practical outcomes (liturgical chaos, doctrinal ambiguity, missionary confusion) appear to them as dysfunction rather than renewal. The post-conciliar Church pays the cost of an institutional arrangement designed to solve a problem the traditionalist reading believes has been artifically perpetuated to justify the constraint's existence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conciliar_compromise_intentionality,
    'To what extent were the documented ambiguities in Vatican II''s texts deliberate compromises designed to enable multiple interpretations, vs. genuine doctrinal confusion or drafting errors?',
    'Archival research into conciliar committee deliberations, personal papers of conciliar drafters, and comparative textual analysis of draft versions (the Acta Synodalia and Riedmatten archives). Interviews with surviving Council fathers or their theological advisors regarding intent.',
    'If ambiguities were deliberately crafted, the traditionalist reading''s framing of the Council as a compromise document enabling heterodoxy is strengthened; the rupture becomes intelligible as a consequence of structural choices made at the Council itself. If ambiguities arose from genuine doctrinal confusion or drafting error, the reading shifts toward framing the Council as well-intentioned but poorly executed, which preserves continuity-reading space (the Council was trying to stay continuous but failed, vs. the rupture was engineered).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conciliar_compromise_intentionality, empirical, 'Whether conciliar textual ambiguities were deliberate structural choices or unintended defects').

omega_variable(
    rupture_vs_development_boundary,
    'Can the post-conciliar institutional changes (suppression of Tridentine liturgy, collapse of missionary theology, doctrinal permissiveness) be understood as organic developments of conciliar doctrine, or do they represent a distinct rupture from what the conciliar texts actually authorize?',
    'Detailed exegetical comparison of conciliar texts against actual post-conciliar implementation; analysis of how later popes (Paul VI, John Paul II) attempted to constrain the ''spirit of the Council'' back toward textual limits; counterfactual analysis of what a purely textual reading of the Council (without the ''spirit'') would have authorized.',
    'If post-conciliar changes exceed what the texts authorize, the rupture is located in the post-conciliar implementation, not the Council itself (weakens the traditionalist reading''s attribution of rupture to the Council). If the texts themselves contain the seeds of the changes (through their ambiguities), the traditionalist reading is strengthened: the Council was the rupture-point. If the implementation represents betrayal of the Council''s actual intent (whether that intent was continuity or measured rupture), the composite_reading gains strength.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rupture_vs_development_boundary, conceptual, 'Whether post-conciliar ruptures are developments authorized by conciliar texts or implementations exceeding textual warrant').

omega_variable(
    identity_lock_mechanism_in_victimhood,
    'For traditionalist clergy and religious (missionary carriers, liturgy defenders), is the experienced suppression structural (canonical prohibitions, institutional barriers) or internalized (beliefs instilled through theological formation that pre-conciliar practice is passé and development is progress)?',
    'Longitudinal trajectory analysis: do suppressed actors (SSPX, underground Tridentine communities, traditionalist seminarians) recover their pre-suppression frameworks when the structural barriers are lifted or softened? If suppression persists after barrier removal, it is partially internalized; if it dissipates, the suppression was primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than measured: victims carry the suppression internally after exit or barrier-lifting. If structural, the constraint''s suppression is accurate at measurement but would collapse with barrier removal. This affects the classification: high internalized suppression suggests snare (victim cannot exit even when barriers lift); high structural suppression with low internalized component suggests tangled_rope (coordination + enforcement, but exit is genuinely possible).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_in_victimhood, empirical, 'Whether suppression of traditionalist voices and practice is structural or internalized in the victims').

omega_variable(
    hermeneutic_reading_dependency,
    'Is the constraintness of Vatican II fundamentally dependent on a PARTICULAR reading (the rupture_traditionalist reading) of its texts, or does the constraint exist independently of reading-choice?',
    'Historical counterfactual: if the continuity_reading had been institutionally dominant post-Council (if the Curia had enforced a stricter continuity hermeneutic), would the constraint exist? Would there still be extraction, suppression, and beneficiaries/victims, or does the constraint dissolve if the reading changes?',
    'If the constraint is reading-dependent, then it is not a property of Vatican II itself but of the hermeneutical choice to read it as rupture and to institutionalize that reading. The rupture_traditionalist_reading instantiates the constraint, but alternative readings would instantiate different constraints (different epsilon, different beneficiary/victim sets). This is the kernel-reading frame''s deep claim: the constraint is READING-INSTANTIATED, not reading-independent. If the constraint persists across all readings, it is reading-independent (a property of the Council''s actual effects, not of the reading imposed on it).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hermeneutic_reading_dependency, conceptual, 'Whether the constraint is a property of Vatican II itself or of the reading instantiated by the traditionalist hermeneutic').

omega_variable(
    false_summit_on_natural_law_status,
    'Is Vatican II itself a ''natural'' institutional event (an inevitable evolution of institutional Christianity), or is the traditionalist reading''s claim that the Council represents rupture actually identifying a CONSTRUCTED constraint that benefits identifiable parties (progressive reformists, episcopal modernizers) who would frame it as natural institutional development?',
    'Comparative institutional analysis: did other major religious institutions (Orthodox churches, Islam, Buddhism, Protestantism) undergo parallel ''conciliar'' modernization in the 1960s, or is Vatican II historically contingent on specific factors (Pope John XXIII''s innovativeness, Cold War dynamics, rapid secularization in the West, theological ferment in European Catholicism)? If contingent, the traditionalist reading may be identifying the false naturalization of a constructed institutional choice.',
    'If Vatican II is institutionally inevitable, the rupture is less a constraint than a natural development, and the traditionalist reading is imposing a normative evaluation (bad rupture) on what is structurally just institutional change. If Vatican II is contingent on specific choices and beneficiaries, the traditionalist reading is correctly identifying a constraint: the institutionalization of particular reformist interests and the suppression of traditionalist alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_on_natural_law_status, empirical, 'Whether Vatican II is an inevitable institutional development or a contingent constructed constraint benefiting identifiable reformist parties').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 1962, 1985).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1962, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 1962, 0.1).
narrative_ontology:measurement(vati_tr_t1965, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 1965, 0.18).
narrative_ontology:measurement(vati_tr_t1968, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 1968, 0.28).
narrative_ontology:measurement(vati_tr_t1973, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 1973, 0.37).
narrative_ontology:measurement(vati_tr_t1978, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 1978, 0.41).
narrative_ontology:measurement(vati_tr_t1985, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 1985, 0.44).

% Extraction over time
narrative_ontology:measurement(vati_be_t1962, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 1962, 0.35).
narrative_ontology:measurement(vati_be_t1965, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 1965, 0.48).
narrative_ontology:measurement(vati_be_t1968, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 1968, 0.61).
narrative_ontology:measurement(vati_be_t1973, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 1973, 0.72).
narrative_ontology:measurement(vati_be_t1978, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 1978, 0.78).
narrative_ontology:measurement(vati_be_t1985, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 1985, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1962, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 1962, 0.25).
narrative_ontology:measurement(vati_su_t1965, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 1965, 0.38).
narrative_ontology:measurement(vati_su_t1968, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 1968, 0.51).
narrative_ontology:measurement(vati_su_t1973, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 1973, 0.62).
narrative_ontology:measurement(vati_su_t1978, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 1978, 0.65).
narrative_ontology:measurement(vati_su_t1985, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 1985, 0.68).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1962, tn=1985
narrative_ontology:measurement(vati_grid_01, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, accessibility_collapse(class), 1962, 0.35).
narrative_ontology:measurement(vati_grid_02, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, accessibility_collapse(class), 1985, 0.74).
narrative_ontology:measurement(vati_grid_03, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, accessibility_collapse(individual), 1962, 0.28).
narrative_ontology:measurement(vati_grid_04, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, accessibility_collapse(individual), 1985, 0.72).
narrative_ontology:measurement(vati_grid_05, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, accessibility_collapse(organizational), 1962, 0.38).
narrative_ontology:measurement(vati_grid_06, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, accessibility_collapse(organizational), 1985, 0.68).
narrative_ontology:measurement(vati_grid_07, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, accessibility_collapse(structural), 1962, 0.42).
narrative_ontology:measurement(vati_grid_08, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, accessibility_collapse(structural), 1985, 0.71).
narrative_ontology:measurement(vati_grid_09, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, resistance(class), 1962, 0.71).
narrative_ontology:measurement(vati_grid_10, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, resistance(class), 1985, 0.52).
narrative_ontology:measurement(vati_grid_11, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, resistance(individual), 1962, 0.58).
narrative_ontology:measurement(vati_grid_12, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, resistance(individual), 1985, 0.68).
narrative_ontology:measurement(vati_grid_13, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, resistance(organizational), 1962, 0.68).
narrative_ontology:measurement(vati_grid_14, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, resistance(organizational), 1985, 0.42).
narrative_ontology:measurement(vati_grid_15, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, resistance(structural), 1962, 0.62).
narrative_ontology:measurement(vati_grid_16, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, resistance(structural), 1985, 0.48).
narrative_ontology:measurement(vati_grid_17, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, stakes_inflation(class), 1962, 0.25).
narrative_ontology:measurement(vati_grid_18, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, stakes_inflation(class), 1985, 0.81).
narrative_ontology:measurement(vati_grid_19, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, stakes_inflation(individual), 1962, 0.18).
narrative_ontology:measurement(vati_grid_20, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, stakes_inflation(individual), 1985, 0.85).
narrative_ontology:measurement(vati_grid_21, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, stakes_inflation(organizational), 1962, 0.28).
narrative_ontology:measurement(vati_grid_22, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, stakes_inflation(organizational), 1985, 0.72).
narrative_ontology:measurement(vati_grid_23, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, stakes_inflation(structural), 1962, 0.32).
narrative_ontology:measurement(vati_grid_24, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, stakes_inflation(structural), 1985, 0.76).
narrative_ontology:measurement(vati_grid_25, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression(class), 1962, 0.21).
narrative_ontology:measurement(vati_grid_26, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression(class), 1985, 0.71).
narrative_ontology:measurement(vati_grid_27, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression(individual), 1962, 0.18).
narrative_ontology:measurement(vati_grid_28, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression(individual), 1985, 0.65).
narrative_ontology:measurement(vati_grid_29, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression(organizational), 1962, 0.19).
narrative_ontology:measurement(vati_grid_30, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression(organizational), 1985, 0.64).
narrative_ontology:measurement(vati_grid_31, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression(structural), 1962, 0.22).
narrative_ontology:measurement(vati_grid_32, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression(structural), 1985, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.12).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority__rupture_progressive_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a contested kernel (vatican_ii_doctrinal_authority). The kernel itself — magisterial authority grounded in conciliar texts — is stable; the readings diverge in their hermeneutical strategy and valuation of conciliar novelty. The rupture_traditionalist_reading interprets the Council's ambiguities as rupture-enabling compromises and attributes post-conciliar heterodoxy to textual permission for innovation; the continuity_reading interprets the same ambiguities as organic development; the rupture_progressive_reading interprets the rupture as salutary and authorizes the 'spirit of the Council' beyond textual limits. All four stories in the family share affected_constraints links. The traditionalist reading's high extractiveness (0.82) reflects the cost borne by traditionalists and doctrinal defenders in exchange for the institutional permission gained by progressives. The progressive reading's extractiveness would be lower (the rupture is valued, not lamented) but the structurally similar victim sets (those who experience suppression of alternatives) would remain. The continuity reading's extractiveness would be lower still (the Council is integrative, not extractive) but the institutional outcomes would be identical — this demonstrates that constraint classification is reading-indexed while outcome-facts remain constant.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, moderate, 0.86).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
