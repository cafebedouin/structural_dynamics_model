% ============================================================================
% CONSTRAINT STORY: vatican_ii_authority__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_authority__rupture_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: vatican_ii_authority__rupture_reading
 *   human_readable: Post-Conciliar Authority Settlement (Rupture Reading)
 *   domain: theology/ecclesiology/religious_authority
 *
 * SUMMARY:
 *   This story instantiates the rupture reading of the Vatican II authority
 *   kernel. Its object is the standing post-conciliar authority settlement:
 *   the requirement of assent to the conciliar documents as binding
 *   magisterial teaching, the licensing and policing of liturgical use, and
 *   the disciplinary machinery that maintains both. Assessed by this
 *   reading's own lights, the settlement imposes teaching it holds to
 *   contradict the prior ordinary magisterium (Dignitatis Humanae on
 *   religious liberty, Nostra Aetate on non-Christian religions, Unitatis
 *   Redintegratio on ecumenism, and the reformed liturgy), sustains itself
 *   through canonical penalty and permission control, and channels
 *   institutional authority to the hierarchy and its theological allies while
 *   traditionalist clergy and laity bear irregularity, restricted access, and
 *   identity loss. Epsilon's referent is this standing arrangement — never
 *   the settlement this reading would erect in its place. The Society of St.
 *   Pius X's position is the reading's paradigmatic instantiation. KEY AGENTS
 *   (by structural relationship): - conciliar_episcopal_hierarchy:
 *   agenda-setter and principal collector ([institutional]/[arbitrage]) —
 *   promulgates, licenses, disciplines - modernist_theological_faction:
 *   primary beneficiary ([institutional]/[arbitrage]) — doctrinal latitude
 *   and institutional position - traditionalist_clergy: primary target
 *   ([organized]/[identity_locked]) — bears canonical irregularity and
 *   suppression - traditional_laity: secondary target
 *   ([moderate]/[constrained]) — bears restricted access, travel, family
 *   division - traditional_catholic_identity: non-agent bearer
 *   ([powerless]/[trapped]) — the inherited doctrinal-liturgical inheritance
 *   the reading charges is being retired -
 *   reservation_holding_diocesan_priests: excluded voice
 *   ([moderate]/[constrained]) — silent under career risk -
 *   comparative_ecclesiology_scholars: analytical observer
 *   ([analytical]/[analytical]) — documents the textual record
 *
 * KEY AGENTS:
 *   - conciliar_episcopal_hierarchy: agenda-setter and principal collector ([institutional]/[arbitrage]) — administers and enforces the settlement
 *   - modernist_theological_faction: primary beneficiary ([institutional]/[arbitrage]) — doctrinal latitude and institutional position
 *   - traditionalist_clergy: primary target ([organized]/[identity_locked]) — bears canonical irregularity and suppression
 *   - traditional_laity: secondary target ([moderate]/[constrained]) — bears restricted access and communal division
 *   - traditional_catholic_identity: non-agent bearer ([powerless]/[trapped]) — the inherited doctrinal-liturgical corpus the reading charges is being dismantled
 *   - reservation_holding_diocesan_priests: excluded voice ([moderate]/[constrained]) — privately dissenting, publicly silent
 *   - comparative_ecclesiology_scholars: analytical observer ([analytical]/[analytical]) — documents draft history, penalties, and reversals
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_authority__rupture_reading, 0.84).
domain_priors:suppression_score(vatican_ii_authority__rupture_reading, 0.78).
domain_priors:theater_ratio(vatican_ii_authority__rupture_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, extractiveness, 0.84).
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_authority__rupture_reading, snare).
narrative_ontology:human_readable(vatican_ii_authority__rupture_reading, "Post-Conciliar Authority Settlement (Rupture Reading)").
narrative_ontology:topic_domain(vatican_ii_authority__rupture_reading, "theology/ecclesiology/religious_authority").

domain_priors:requires_active_enforcement(vatican_ii_authority__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_authority__rupture_reading, '2971c8df-5566-46a1-a8c8-ad4c8f5408c8').
narrative_ontology:cs_kernel_codification('2971c8df-5566-46a1-a8c8-ad4c8f5408c8', fixed_text).
narrative_ontology:cs_authority_grounding('2971c8df-5566-46a1-a8c8-ad4c8f5408c8', extraction).
narrative_ontology:cs_interpretation_layer_present('2971c8df-5566-46a1-a8c8-ad4c8f5408c8').
narrative_ontology:cs_reading_relation('2971c8df-5566-46a1-a8c8-ad4c8f5408c8', vatican_ii_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('2971c8df-5566-46a1-a8c8-ad4c8f5408c8', vatican_ii_authority__composite_overdetermination_reading, forecloses).
narrative_ontology:cs_axiom('2971c8df-5566-46a1-a8c8-ad4c8f5408c8', foundational, conciliar_documents_contain_genuine_doctrinal_error).
narrative_ontology:cs_axiom_status(conciliar_documents_contain_genuine_doctrinal_error, holdable).
narrative_ontology:cs_axiom_grounding('2971c8df-5566-46a1-a8c8-ad4c8f5408c8', conciliar_documents_contain_genuine_doctrinal_error, empirically_contingent).
narrative_ontology:cs_axiom('2971c8df-5566-46a1-a8c8-ad4c8f5408c8', foundational, prior_magisterium_binds_against_conciliar_novelty).
narrative_ontology:cs_axiom_status(prior_magisterium_binds_against_conciliar_novelty, holdable).
narrative_ontology:cs_axiom_grounding('2971c8df-5566-46a1-a8c8-ad4c8f5408c8', prior_magisterium_binds_against_conciliar_novelty, deontological).
narrative_ontology:cs_reference_frame('2971c8df-5566-46a1-a8c8-ad4c8f5408c8', pre_conciliar_magisterial_settlement).
narrative_ontology:cs_drift_state('2971c8df-5566-46a1-a8c8-ad4c8f5408c8', contemporary_post_conciliar_implementation, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('2971c8df-5566-46a1-a8c8-ad4c8f5408c8', '').
narrative_ontology:cs_kernel_id(vatican_ii_authority__rupture_reading, vatican_ii_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_authority__rupture_reading, modernist_theological_faction).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__rupture_reading, conciliar_episcopal_hierarchy).
narrative_ontology:constraint_victim(vatican_ii_authority__rupture_reading, traditionalist_clergy).
narrative_ontology:constraint_victim(vatican_ii_authority__rupture_reading, traditional_laity).
narrative_ontology:constraint_victim(vatican_ii_authority__rupture_reading, traditional_catholic_identity).
narrative_ontology:constraint_vindicates(vatican_ii_authority__rupture_reading, dignitatis_humanae_religious_liberty).
narrative_ontology:constraint_vindicates(vatican_ii_authority__rupture_reading, nostra_aetate_pluralist_engagement).
narrative_ontology:constraint_vindicates(vatican_ii_authority__rupture_reading, unitatis_redintegratio_ecumenism).
narrative_ontology:constraint_vindicates(vatican_ii_authority__rupture_reading, novus_ordo_liturgical_settlement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Theologians and curial staff who drafted and promoted the conciliar texts and afterward filled post-conciliar faculties, commissions, and editorial chairs. The settlement gives them doctrinal latitude their predecessors were denied under early-twentieth-century censures, plus control of the interpretive apparatus. Nothing about their position pushes them to leave; they shape the terms they live under.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, modernist_theological_faction, beneficiary,
    institutional, generational, arbitrage, global).

% Roman congregations and the episcopate that promulgate and police the settlement: they require assent to conciliar teaching, license liturgical use, discipline dissenting clerics, and appoint successors. The arrangement concentrates jurisdiction and administrative reach in their hands, and they collect obedience, resources, and legitimacy through it. As rule-setters they face no external alternative to govern by.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, conciliar_episcopal_hierarchy, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_authority__rupture_reading, conciliar_episcopal_hierarchy, beneficiary).

% Priests formed before or against the settlement — Society of St. Pius X districts, allied institutes, and independent traditional chapels. They celebrate the pre-conciliar rites under canonical irregularity or suppression, ordain through their own seminaries, and bear suspensions, interdicts, and property losses. Their priestly identity was formed around rejecting the settlement; taking diocesan incardination on the settlement's terms would mean repudiating the vocation as they understand it.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, traditionalist_clergy, payer,
    organized, biographical, identity_locked, global).

% Laypeople attached to the traditional liturgy and catechesis. They absorb reduced access as permissions are withdrawn, travel past closed parishes to irregular chapels, split families and parishes over attendance, and fund parallel schools. Alternatives exist — indult communities, Eastern parishes, other communions — but each carries real costs of distance, community loss, or changed belief.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, traditional_laity, payer,
    moderate, biographical, constrained, global).

% Not an actor: the inherited body of pre-conciliar doctrine, liturgy, and devotional practice that the settlement re-reads or retires. It bears the change the way a language bears translation — listed because the reading's complaint centers on it, and it collects nothing and decides nothing.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, traditional_catholic_identity, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(vatican_ii_authority__rupture_reading, traditional_catholic_identity).

% Diocesan priests who privately doubt that the settlement coheres with prior teaching but continue in parish ministry. Voicing doubts risks assignments, advancement, and faculties, so they keep reservations out of synodal consultations and off the record; their silence is the price of staying employed in ministry.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, reservation_holding_diocesan_priests, excluded,
    moderate, biographical, constrained, national).

% Academic historians and canonists who document the dispute's textual record — draft evolution, the footnote controversies, the 1988 excommunications and their 2009 lifting, the permission regime's reversals. They collect no obedience and pay no penalties; both sides cite their work.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, comparative_ecclesiology_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_authority__rupture_reading, conciliar_episcopal_hierarchy).
narrative_ontology:fixing_cost_class(vatican_ii_authority__rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single worldwide governance and liturgical framework for the Roman Catholic Church after a sweeping internal reform: common rites, common catechetical norms, collegial structures linking Rome to episcopal conferences, and a defined assent owed to conciliar teaching.
% TRANSFER_FUNCTION: Moves doctrinal assent and liturgical conformity from clergy and laity toward the conciliar settlement; moves institutional authority, appointments, and legitimacy from holders of the pre-conciliar formation to the post-conciliar hierarchy and its theological allies.
% ABSENT_VOICES: Pre-conciliar magisterial texts (Mirari Vos, Quanta Cura, Pascendi, Mortalium Animos) cannot testify in synodal assemblies; reservation-holding diocesan priests stay silent under career risk; traditional laity lack seats in synodal processes; dissenting conciliar fathers' reservations survive mainly in private diaries. They are located outside the consultative machinery — in suppressed communities, in the archive, and in parishes where speaking carries assignment consequences.
% DISAPPEARANCE_RATIONALE: Overnight removal would regularize the SSPX immediately, restore unrestricted traditional liturgy, dissolve the assent machinery, and strip the post-conciliar hierarchy of the disciplinary instruments that maintain the settlement; the modernist faction would lose the institutional positions that depend on it.
% FOUNDING_PROBLEM: How the Roman Catholic Church could engage modern constitutional states, religious pluralism, and critical scholarship without losing the deposit of faith it claims to guard — the problem framed by the modernist crisis and the long standoff between the Syllabus era and twentieth-century pluralism.
% FOUNDING_PROBLEM_CORROBORATION: Secular historians of the modernist crisis and of the Council — outside any benefiting party — attest the engagement problem was real; the traditionalist opposition itself concedes the pastoral problem existed while disputing the remedy; no party inside the settlement disputes that the problem was live at the time. The dispute is over whether the adopted solution preserved or betrayed the deposit, not over whether the problem existed.
narrative_ontology:disappearance_verdict(vatican_ii_authority__rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_authority__rupture_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_authority__rupture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_authority__rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_authority__rupture_reading, 0.84, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_authority__rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_authority__rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_authority__rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Scores are reading-indexed to the rupture seat over the standing arrangement. Extractiveness 0.84: assent to teaching this reading holds erroneous, required of the whole faithful. Suppression 0.78: canonical penalties, permission regimes, and appointment control — a raw structural measure, unscaled by power or scope. Theater 0.52: pastoral and unity language carries a large share of the load while the operative mechanism is disciplinary; that share grew as enforcement alternated with détente. Accessibility collapse 0.55: alternatives persist (irregular chapels, indult communities, other communions) but each is costly. Resistance 0.72: organized, global, durable. The temporal series run on one shared nine-point grid (1962–2026) and trace a crackdown–détente cycle: enforcement peaks at the 1988 excommunications, relaxes through Summorum Pontificum (2007), re-tightens under Traditionis Custodes (2021). The oscillation is itself part of the mechanism — each détente raises expectations among traditionalists and each reversal deepens their lock-in, an intermittent-reinforcement pattern rather than noise. Base_properties report the end-state (late-cycle, re-tightened) phase. The claimed type is authored independently from the metrics: from this seat the settlement is a snare — the coordination story (unity, pastoral renewal) is, on this reading, cover for enforced assent to error, and persistence depends on coercion and on pricing exits.
 *
 * PERSPECTIVAL GAP:
 *   From the hierarchy's seat the settlement is the governance it stewards: unity, common rites, collegial order — the same structure computes as coordination from that chair. From the traditionalist clergy's seat the identical structure is enforced assent to error, with exit priced in identity. Reservation-holding priests occupy a third position: outward compliance, inward dissent, no seat in the conversation. The engine computes these divergent per-seat classifications from the structural data; the authored snare claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The modernist faction and the hierarchy sit near the beneficiary end: the settlement subsidizes their authority and latitude, and the hierarchy's rule-setting position gives it arbitrage-grade distance from any cost it dislikes. Traditionalist clergy sit nearest the full-target end — identity_locked exit amplifies their exposure beyond what canonical penalty alone would produce. Traditional laity sit high but below clergy: constrained exit (indult communities, other communions) damps effective extraction slightly. The non-agent identity entry is excluded from directionality arithmetic (agent:false), mirroring the principle that a non-actor must not feed d→χ as if it collected or paid. Suppression is authored as a raw structural property and is not scaled; only extractiveness is scaled, by directionality and spatial scope (global scope modestly amplifies verification difficulty).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — engaging modernity without losing the deposit — is still live, so no mandatrophy resolution applies; the arrangement is contested over legitimacy, not obsolescence. Classifying from this seat as a snare prevents the mislabel the continuity frame invites (pure unity-coordination), while the omegas keep the classification honest: if the contradiction claims fail textual scrutiny, epsilon collapses toward the coordination floor and the type migrates toward rope or tangled_rope; if the modernist faction proves rhetorically diffuse, capture narrows to the hierarchy alone. The R5 mismatch check (status live × verdict world_rearranges) raises no zombie flag — the dispute is over whether the mandate was ever legitimately held, not whether it expired.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This constraint is one reading (rupture_reading) of kernel vatican_ii_authority; what structural deltas would the sibling readings introduce?',
    'Read the sibling files (vatican_ii_authority__continuity_reading, vatican_ii_authority__composite_overdetermination_reading): the continuity reading collapses epsilon toward the coordination floor and empties the victim set; the composite reading denies a determinate type and splits the arrangement into sub-constraints.',
    'Classification is reading-indexed; adopting a sibling''s frame changes epsilon, victims, and type wholesale rather than adjusting a parameter.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer structure: which kernel, which reading, what siblings would change.').

omega_variable(
    contradiction_or_development_location,
    'Do the conciliar texts on religious liberty, non-Christian religions, and ecumenism contradict the prior ordinary magisterium, or develop it? This is the located disagreement between this reading and the continuity reading.',
    'Textual-historical adjudication: draft evolution, the 1965 relatio, papal glosses, and semantic analysis of key terms (rights of the person versus the confessional state; subsistit versus est).',
    'Genuine contradiction sustains this reading''s epsilon and its snare claim; successful harmonization collapses epsilon toward the coordination floor and relocates the dispute to interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contradiction_or_development_location, conceptual, 'Where the rupture-versus-continuity disagreement is textually located.').

omega_variable(
    council_validity_and_defectibility,
    'Were the Council''s acts validly promulgated, and can an ecumenical council err in its doctrinal affirmations?',
    'Canonical-theological adjudication of promulgation form and of the scope of ecclesial defectibility; no external court exists to settle it.',
    'Invalidity reframes the arrangement as usurpation rather than defective authority; affirmed validity forces this reading to recast its charge as protest against interpretation rather than error in the texts themselves.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(council_validity_and_defectibility, conceptual, 'Validity and defectibility premises underlying the rupture charge.').

omega_variable(
    modernist_faction_coherence,
    'Is the modernist theological faction a coordinated actor that captures the settlement''s gains, or a retrospective label spread over heterogeneous careers?',
    'Prosopography of the conciliar periti and post-conciliar appointment networks; citation and patronage tracing across faculties, congregations, and editorial chairs.',
    'A coherent faction supports naming it a beneficiary seat alongside the hierarchy; a diffuse one narrows gain_flow to the hierarchy alone and shifts the reading from factional capture to institutional self-dealing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modernist_faction_coherence, empirical, 'Whether the declared beneficiary is one actor or many.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression structural (canonical penalties, permission regimes, appointment control) or internalized (traditionalist identity fused with resistance, making exit unthinkable even where tolerated alternatives exist)?',
    'Post-exit trajectory study: traditionalists who move to indult communities, Eastern parishes, or other communions — does the felt compulsion persist after the barrier is removed?',
    'An internalized component raises effective suppression above the structural measure and strengthens the identity_locked exit coding; a purely structural profile predicts rapid normalization if penalties lapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized composition of the suppression measure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_authority__rupture_reading, 1962, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1962, vatican_ii_authority__rupture_reading, theater_ratio, 1962, 0.3).
narrative_ontology:measurement_basis(vati_tr_t1962, observed).
narrative_ontology:measurement(vati_tr_t1970, vatican_ii_authority__rupture_reading, theater_ratio, 1970, 0.4).
narrative_ontology:measurement_basis(vati_tr_t1970, observed).
narrative_ontology:measurement(vati_tr_t1978, vatican_ii_authority__rupture_reading, theater_ratio, 1978, 0.44).
narrative_ontology:measurement_basis(vati_tr_t1978, observed).
narrative_ontology:measurement(vati_tr_t1988, vatican_ii_authority__rupture_reading, theater_ratio, 1988, 0.46).
narrative_ontology:measurement_basis(vati_tr_t1988, observed).
narrative_ontology:measurement(vati_tr_t1995, vatican_ii_authority__rupture_reading, theater_ratio, 1995, 0.45).
narrative_ontology:measurement_basis(vati_tr_t1995, observed).
narrative_ontology:measurement(vati_tr_t2007, vatican_ii_authority__rupture_reading, theater_ratio, 2007, 0.42).
narrative_ontology:measurement_basis(vati_tr_t2007, observed).
narrative_ontology:measurement(vati_tr_t2013, vatican_ii_authority__rupture_reading, theater_ratio, 2013, 0.46).
narrative_ontology:measurement_basis(vati_tr_t2013, observed).
narrative_ontology:measurement(vati_tr_t2021, vatican_ii_authority__rupture_reading, theater_ratio, 2021, 0.5).
narrative_ontology:measurement_basis(vati_tr_t2021, observed).
narrative_ontology:measurement(vati_tr_t2026, vatican_ii_authority__rupture_reading, theater_ratio, 2026, 0.52).
narrative_ontology:measurement_basis(vati_tr_t2026, projected).

% Extraction over time
narrative_ontology:measurement(vati_be_t1962, vatican_ii_authority__rupture_reading, base_extractiveness, 1962, 0.35).
narrative_ontology:measurement_basis(vati_be_t1962, observed).
narrative_ontology:measurement(vati_be_t1970, vatican_ii_authority__rupture_reading, base_extractiveness, 1970, 0.62).
narrative_ontology:measurement_basis(vati_be_t1970, observed).
narrative_ontology:measurement(vati_be_t1978, vatican_ii_authority__rupture_reading, base_extractiveness, 1978, 0.68).
narrative_ontology:measurement_basis(vati_be_t1978, observed).
narrative_ontology:measurement(vati_be_t1988, vatican_ii_authority__rupture_reading, base_extractiveness, 1988, 0.74).
narrative_ontology:measurement_basis(vati_be_t1988, observed).
narrative_ontology:measurement(vati_be_t1995, vatican_ii_authority__rupture_reading, base_extractiveness, 1995, 0.72).
narrative_ontology:measurement_basis(vati_be_t1995, observed).
narrative_ontology:measurement(vati_be_t2007, vatican_ii_authority__rupture_reading, base_extractiveness, 2007, 0.7).
narrative_ontology:measurement_basis(vati_be_t2007, observed).
narrative_ontology:measurement(vati_be_t2013, vatican_ii_authority__rupture_reading, base_extractiveness, 2013, 0.73).
narrative_ontology:measurement_basis(vati_be_t2013, observed).
narrative_ontology:measurement(vati_be_t2021, vatican_ii_authority__rupture_reading, base_extractiveness, 2021, 0.8).
narrative_ontology:measurement_basis(vati_be_t2021, observed).
narrative_ontology:measurement(vati_be_t2026, vatican_ii_authority__rupture_reading, base_extractiveness, 2026, 0.84).
narrative_ontology:measurement_basis(vati_be_t2026, projected).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1962, vatican_ii_authority__rupture_reading, suppression_requirement, 1962, 0.25).
narrative_ontology:measurement_basis(vati_su_t1962, observed).
narrative_ontology:measurement(vati_su_t1970, vatican_ii_authority__rupture_reading, suppression_requirement, 1970, 0.55).
narrative_ontology:measurement_basis(vati_su_t1970, observed).
narrative_ontology:measurement(vati_su_t1978, vatican_ii_authority__rupture_reading, suppression_requirement, 1978, 0.6).
narrative_ontology:measurement_basis(vati_su_t1978, observed).
narrative_ontology:measurement(vati_su_t1988, vatican_ii_authority__rupture_reading, suppression_requirement, 1988, 0.78).
narrative_ontology:measurement_basis(vati_su_t1988, observed).
narrative_ontology:measurement(vati_su_t1995, vatican_ii_authority__rupture_reading, suppression_requirement, 1995, 0.68).
narrative_ontology:measurement_basis(vati_su_t1995, observed).
narrative_ontology:measurement(vati_su_t2007, vatican_ii_authority__rupture_reading, suppression_requirement, 2007, 0.58).
narrative_ontology:measurement_basis(vati_su_t2007, observed).
narrative_ontology:measurement(vati_su_t2013, vatican_ii_authority__rupture_reading, suppression_requirement, 2013, 0.66).
narrative_ontology:measurement_basis(vati_su_t2013, observed).
narrative_ontology:measurement(vati_su_t2021, vatican_ii_authority__rupture_reading, suppression_requirement, 2021, 0.75).
narrative_ontology:measurement_basis(vati_su_t2021, observed).
narrative_ontology:measurement(vati_su_t2026, vatican_ii_authority__rupture_reading, suppression_requirement, 2026, 0.78).
narrative_ontology:measurement_basis(vati_su_t2026, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_authority__rupture_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(vatican_ii_authority__rupture_reading, vatican_ii_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__rupture_reading, vatican_ii_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% Constraint family: one kernel (vatican_ii_authority), three readings, three files. The colloquial label 'Vatican II's authority' conflates structurally distinct commitments: the continuity reading authors low epsilon with no victims; the composite reading declines a determinate verdict and decomposes the arrangement; the rupture reading (this file) authors high epsilon with named victims and active enforcement. Direction of influence: the continuity reading supplies the official gloss this reading argues against; the composite reading's documentation of incompatible rationales feeds rupture arguments. Each file links the others via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
