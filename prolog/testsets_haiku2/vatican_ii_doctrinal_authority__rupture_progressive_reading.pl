% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__rupture_progressive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_doctrinal_authority__rupture_progressive_reading, []).

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
 *   constraint_id: vatican_ii_doctrinal_authority__rupture_progressive_reading
 *   human_readable: Vatican II Rupture-Progressive Authority: Conciliar Mandate as Ongoing Reform Authorization
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   This constraint instantiates the progressive-rupture reading of Vatican
 *   II: the Council represents a necessary break with pre-conciliar
 *   institutional rigidity, and the 'spirit of the Council' — the intuited
 *   intent driving the reforms, not merely the documents' explicit text —
 *   authorizes ongoing doctrinal and disciplinary transformation beyond the
 *   Council's formal language. This reading has dominated post-conciliar
 *   implementation through papal, episcopal, and theological leadership. It
 *   coordinates genuine institutional reform (lay participation, ecumenical
 *   engagement, pastoral flexibility) with asymmetric extraction: progressive
 *   reform constituencies gain interpretive authority and institutional
 *   momentum, while traditionalist clergy and conservation constituencies
 *   bear the cost of having their entire formation and practice delegitimized
 *   as obsolete. The constraint is CLAIMED as tangled_rope (coordination of
 *   genuine reform + extraction through asymmetric hermeneutical authority)
 *   and the authored metrics track rising extractiveness as the reading
 *   accumulates institutional power and rising theater as the 'spirit of the
 *   Council' becomes itself a ritualized justification for decisions no
 *   longer traceable to the documents' language.
 *
 * KEY AGENTS:
 *   - Progressive reform theologians (beneficiary, institutional power) — shape implementation through theological interpretation and episcopal influence; their career advancement rides on the reading that reform realizes conciliar intent.
 *   - Traditionalist clergy (payer, moderate power, identity-locked) — their entire formation is delegitimized; exit requires leaving the priesthood or religious life.
 *   - Episcopal reformers (beneficiary, powerful) — justify pastoral innovations by appeal to conciliar intent even when texts are ambiguous or restrictive.
 *   - Papal magisterium (agenda-setter, institutional) — adjudicates between readings but remains internally divided across pontificates.
 *   - Pre-conciliar institutional apparatus (payer, institutional, trapped) — scholastic theology, Latin liturgy, disciplinary regimes are dismantled by reading-driven implementation.
 *   - Lay traditionalist constituencies (payer, organized, constrained) — efforts to preserve 1962 forms face constant institutional pressure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.68).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.45).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__rupture_progressive_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__rupture_progressive_reading, "Vatican II Rupture-Progressive Authority: Conciliar Mandate as Ongoing Reform Authorization").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__rupture_progressive_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__rupture_progressive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__rupture_progressive_reading, 'c6cef2cc-3d17-4efe-a24b-0c5d5d653fc0').
narrative_ontology:cs_kernel_codification('c6cef2cc-3d17-4efe-a24b-0c5d5d653fc0', fixed_text).
narrative_ontology:cs_authority_grounding('c6cef2cc-3d17-4efe-a24b-0c5d5d653fc0', lineage).
narrative_ontology:cs_interpretation_layer_present('c6cef2cc-3d17-4efe-a24b-0c5d5d653fc0').
narrative_ontology:cs_reading_relation('c6cef2cc-3d17-4efe-a24b-0c5d5d653fc0', vatican_ii_doctrinal_authority__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('c6cef2cc-3d17-4efe-a24b-0c5d5d653fc0', vatican_ii_doctrinal_authority__rupture_traditionalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('c6cef2cc-3d17-4efe-a24b-0c5d5d653fc0', vatican_ii_doctrinal_authority__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('c6cef2cc-3d17-4efe-a24b-0c5d5d653fc0', foundational, conciliar_break_with_preconciliar_rigidity).
narrative_ontology:cs_axiom_status(conciliar_break_with_preconciliar_rigidity, holdable).
narrative_ontology:cs_axiom_grounding('c6cef2cc-3d17-4efe-a24b-0c5d5d653fc0', conciliar_break_with_preconciliar_rigidity, deontological).
narrative_ontology:cs_axiom('c6cef2cc-3d17-4efe-a24b-0c5d5d653fc0', foundational, spirit_as_generative_beyond_letter).
narrative_ontology:cs_axiom_status(spirit_as_generative_beyond_letter, holdable).
narrative_ontology:cs_axiom_grounding('c6cef2cc-3d17-4efe-a24b-0c5d5d653fc0', spirit_as_generative_beyond_letter, conventional).
narrative_ontology:cs_axiom('c6cef2cc-3d17-4efe-a24b-0c5d5d653fc0', secondary, ongoing_reform_realizes_rather_than_betrays_conciliar_intent).
narrative_ontology:cs_axiom_status(ongoing_reform_realizes_rather_than_betrays_conciliar_intent, holdable).
narrative_ontology:cs_axiom_grounding('c6cef2cc-3d17-4efe-a24b-0c5d5d653fc0', ongoing_reform_realizes_rather_than_betrays_conciliar_intent, instrumental).
narrative_ontology:cs_reference_frame('c6cef2cc-3d17-4efe-a24b-0c5d5d653fc0', vatican_ii_as_authorized_break_with_preconciliar_forms).
narrative_ontology:cs_drift_state('c6cef2cc-3d17-4efe-a24b-0c5d5d653fc0', contemporary_post_benedict_xvi_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c6cef2cc-3d17-4efe-a24b-0c5d5d653fc0', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, progressive_reform_theologians).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, ecumenical_advocates).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, episcopal_reformers).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, traditionalist_clergy).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, pre_conciliar_institutional_apparatus).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, doctrinal_conservation_constituencies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, lay_constituencies_pluralistic).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Theologians and bishops who read the Council's documents as authorization for substantive doctrinal evolution beyond their letter. They claim the 'spirit of the Council' — the intuition driving the reforms, not merely the codified text — as the authentic conciliar mandate. They shape implementation through theological interpretation, academic influence, and episcopal appointment, treating ambiguities in conciliar language as intentional openings. Their career advancement, institutional influence, and publication record depend on the reading that ongoing reform realizes rather than betrays conciliar intent.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, progressive_reform_theologians, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__rupture_progressive_reading, progressive_reform_theologians, agenda_setter).

% Vatican II's turn toward other Christian traditions and secular modernity is read by ecumenicalists as a structural reorientation, not a tactical gesture. They benefit from the 'spirit' reading because it authorizes deepening engagement with non-Catholic bodies and reformulation of boundary doctrines. A tight textualist reading that confines change to the documents' letter would constrain their initiatives.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, ecumenical_advocates, beneficiary,
    institutional, biographical, constrained, global).

% Bishops implementing conciliar changes in their dioceses — liturgical reform, seminary education, parish structures — who justify innovations by appeal to conciliar intent even when the documents contain ambiguous or restrictive language on specific points. The 'spirit' reading grants them interpretive authority to navigate local pastoral needs against Rome's strictures.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, episcopal_reformers, beneficiary,
    powerful, biographical, constrained, national).

% Priests and religious formed in pre-conciliar theology and practice who experience the 'spirit of the Council' narrative as delegitimizing their entire formation, ministry, and understanding of the faith. They are told they embody rigidity and that the Council authorized moving beyond what they were taught as immutable. Their pastoral authority is undermined by the reading that reimagines the Council as a break rather than development. Exit means leaving religious life or the priesthood, a cost amplified by age and vocational identity.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, traditionalist_clergy, payer,
    moderate, biographical, identity_locked, local).

% The scholastic theological method, Latin liturgy, clerical discipline regimes, and doctrinal manuals that structured pre-conciliar Catholicism. These are not agents but institutional arrangements whose dismantling is implied by the rupture reading. They are 'victims' in the sense that the reading authorizes their replacement, not preservation. Their persistence becomes a resistance point the reading must suppress.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, pre_conciliar_institutional_apparatus, payer,
    institutional, generational, trapped, global).

% Lay traditionalist movements, seminaries, and dioceses committed to pre-conciliar forms of piety and doctrine. They experience the 'spirit of the Council' as constantly expanding grounds for dismantling what they hold as essential. Their efforts to preserve 1962-form Latin Mass, scholastic theology, and strict moral disciplines are read by progressives not as legitimate alternatives but as refusals to accept the Council's mandate for rupture. They remain within institutional structures but in permanent friction with the reading.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, doctrinal_conservation_constituencies, payer,
    organized, biographical, constrained, global).

% The Pope and Roman Curia adjudicate between competing readings of Vatican II and claim authority to determine conciliar meaning. Successive popes have adopted the rupture-progressive reading (Paul VI, John Paul II in phases, Francis) or moved toward the continuity reading (John Paul II later, Benedict XVI), creating doctrinal turbulence. Their rulings shape which theologians are censured, which bishops are appointed, and which interpretations gain institutional force.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, papal_magisterium, agenda_setter,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__rupture_progressive_reading, papal_magisterium, observer).

% Catholic lay members who welcomed the Council's opening to modernity, women's dignity, conscience formation, and engagement with secular knowledge. They benefit from the 'spirit of the Council' reading because it legitimizes their lived practice — remarried-divorced reception of communion, contraception use, women in parish leadership — as authentic development, not transgression. Without the rupture reading they would face constant institutional pressure to return to pre-conciliar discipline.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, lay_constituencies_pluralistic, beneficiary,
    powerless, biographical, mobile, national).

% The 16 conciliar texts themselves, treated here as an artifact rather than an actor. Their language exhibits genuine ambiguity on central points (liturgical reform, episcopal collegiality, religious freedom, relations with non-Catholic traditions). Different readings extract different mandates from the same texts because the texts permit it. The documents are the fixed kernel the readings contest; they do not adjudicate between readings.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_documents, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_documents).

% Theologians and bishops holding the competing continuity reading — Vatican II as organic development, not rupture — are structurally excluded from this story because the rupture-progressive reading does not directly engage them as parties; they are a competing reading that would be authored as a separate constraint. Nonetheless their exclusion from influence is maintained through institutional dynamics that privilege progressive momentum.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, continuity_reading_constituency, excluded,
    institutional, generational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_doctrinal_authority__rupture_progressive_reading, progressive_reform_theologians).
narrative_ontology:fixing_cost_class(vatican_ii_doctrinal_authority__rupture_progressive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the structural crisis facing the post-WWII Catholic Church: rigidity in response to modernity was producing institutional atrophy, seminary dropout, loss of educated lay constituency. Vatican II coordinated a reorientation toward contemporary thought, ecumenical engagement, and pastoral flexibility. The 'spirit of the Council' reading frames this not as episodic reform but as authorization for ongoing adaptive reinterpretation as pastoral circumstances require.
% TRANSFER_FUNCTION: Transfers interpretive authority from fixed doctrinal manuals and scholastic theology to living theological reflection and episcopal discretion. Transfers liturgical authority from Roman standardization to vernacular and local adaptation. Transfers the locus of doctrinal development from defensive conservation to dynamic engagement with modern knowledge and ecumenical conversation. This movement of authority is coordinated by the reading that the Council intended rupture, not mere modification.
% ABSENT_VOICES: Pre-conciliar theological traditions treated as obsolete rather than preserved are absent — the reading itself silences them. Traditionalist-reading proponents would argue the Council's ambiguities and errors enabled destructive heterodoxy, but this reading explicitly excludes that objection as a refusal to accept the Council's mandate. Lay constituencies outside educated metropolitan Catholicism — rural, immigrant, pre-modern communities — are absent from the theological conversation the reading drives.
% DISAPPEARANCE_RATIONALE: If the 'spirit of the Council' reading and its institutional mechanisms evaporated, the Church would revert to a textualist reading of Vatican II documents that would constrain rather than authorize ongoing reform. Seminary formation would stabilize around the documented decisions, not their speculated intent. Lay participation in parish governance would recede. Ecumenical engagement would narrow. The post-conciliar theological ferment that produced liberation theology, feminist theology, and contextual pastoral adaptation would have no institutional anchor. Progressive bishops would lose the hermeneutical ground for liturgical experimentation. The Church would not return to 1962, but it would arrest its trajectory of continuous transformation.
% FOUNDING_PROBLEM: Pre-conciliar institutional rigidity in response to post-Enlightenment modernity produced doctrinal sclerosis, seminary crisis, educated-lay alienation, and institutional irrelevance to contemporary consciousness. The Council was called to address this atrophy by opening the Church to historical development and modern thought without abandoning doctrinal integrity.
% FOUNDING_PROBLEM_CORROBORATION: Progressive bishops and theologians attested in conciliar debates that the pre-conciliar stance was untenable; Vatican II's own opening address (John XXIII) affirmed the need for aggiornamento. Academic historians outside the benefiting parties (Catholic, Protestant, secular) concur that institutional crisis was genuine in 1962-1968. However, by 2024, no serious Catholic voice argues for return to pre-conciliar rigidity — even conservatives accept the Council's basic reformations. The founding problem is attested as live in 1962-1975 but dead by 2001-2024. Yet the institutional mechanisms of the 'spirit of the Council' reading persist despite the founding problem's resolution, which is the mandatrophy signature.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__rupture_progressive_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__rupture_progressive_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__rupture_progressive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__rupture_progressive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__rupture_progressive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_progressive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_doctrinal_authority__rupture_progressive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises over time (0.15→0.68 across the interval) because the reading gradually accumulates institutional power: theologians holding it gain university chairs, bishops holding it are appointed to influential sees, papal authority shifts to embrace the reading. At t=1962, the reading is nascent and extractive mostly of pre-conciliar authority. By t=1975 (post-Paul VI adoption), it has institutional momentum. By t=1988 (after John Paul II's mixed embrace), it is dominant in implementation even where papal theology wavers. By t=2001-2024, it is so institutionalized that counter-readings are minority positions. Theater ratio rises then stabilizes (0.25→0.54, flat 2001-2024) because ritualized invocation of the 'spirit' becomes the justification for decisions no longer traceable to the documents themselves — by 2024, the 'spirit' language is performative rather than hermeneutical, used to consecrate institutional momentum. Suppression requirement rises then plateaus (0.15→0.45) because traditionalist resistance must be continuously managed through institutional gatekeeping but has stabilized into a permanent minority position rather than a growing challenge.
 *
 * PERSPECTIVAL GAP:
 *   The divergence in seat perception arises from hermeneutical authority asymmetry: progressive beneficiaries control the interpretive apparatus (theology, publication, episcopal appointment) and thus define what counts as 'rigidity' vs. authentic reform. Traditionalist payers have no competing hermeneutical apparatus — their objections are pre-empted as defenses of obsolescence. The constraint extracts because one reading's authority over interpretation creates permanent winners and losers, not because the underlying coordination problem (how to relate doctrinal stability to historical change) is genuinely solved.
 *
 * DIRECTIONALITY LOGIC:
 *   Extraction accelerates during 1962-1988 (the reading moves from nascent to dominant) and stabilizes 1988-2024 (the reading's institutional dominance hardens but faces no displacement threat). The theater ratio's rise-and-plateau pattern reflects: early period = genuine hermeneutical contestation over the Council's meaning; middle period = ritualized invocation of the 'spirit' becoming justification independent of textual warrant; late period = the 'spirit of the Council' is institutional theater, used to consecrate decisions that now have their own momentum. This is classic Piton drift: the original coordination function (resolving institutional crisis through adaptive reinterpretation) persists as institutional narrative, but the actual operation is extraction maintained through gatekeeping and performance.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits mandatrophy symptoms: the founding problem (pre-conciliar rigidity producing institutional crisis) was genuine and urgent in 1962. By 1975 it was substantially solved: the Church had reformed its liturgy, opened to ecumenism, begun integrating modern knowledge, and retained educated lay participation. By 2001-2024, the founding problem was dead (no serious Catholic argues for a return to pre-conciliar rigidity), yet the reading persists and indeed has intensified as theater. This is the mandatrophy signature: the rationale that justified the constraint has evaporated but the constraint's institutional mechanisms — interpretive gatekeeping, authority asymmetry, suppression of counter-readings — remain in place. The constraint now persists not because it solves the founding problem but because beneficiary constituencies (progressive theologians, reform bishops) have built careers and identities around it. The theater ratio plateau at 0.52+ (performing the 'spirit of the Council' as justification for routine decisions) indicates the reading is now held by inertia and institutional interest, not by living hermeneutical argument.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    spirit_vs_letter_hermeneutics,
    'Does the Council''s appeal to its own ''spirit'' represent a legitimate hermeneutical principle, or does it license interpretive overreach unconstrained by the documents'' actual language?',
    'Historical-linguistic analysis of the Council''s debates and drafting process to determine whether ambiguities were intentional (supporting ''spirit'' reading) or unintended (supporting textualist constraint). Examination of whether post-conciliar developments trace to conciliar intent or extrapolate beyond it.',
    'If ambiguities were intentional, the ''spirit of the Council'' is a legitimate reading principle and extraction is the price of adaptive institutional reform. If unintended, the reading is hermeneutical overreach and the constraint is snare rather than tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(spirit_vs_letter_hermeneutics, empirical, 'Whether doctrinal ambiguity in Vatican II documents was intentional opening or accident.').

omega_variable(
    coordination_vs_capture_asymmetry,
    'Does the ''spirit of the Council'' reading genuinely solve the crisis of institutional rigidity, or does it solve it for beneficiary constituencies while extracting from traditionalist payers who lose interpretive voice?',
    'Comparison of post-conciliar pastoral outcomes (lay participation, seminary health, missionary effectiveness, doctrinal coherence) under progressive implementation vs. counterfactual scenarios under continuity or traditionalist reading. Evaluation of whether traditionalist clergy could have contributed adaptive wisdom if included rather than suppressed.',
    'If genuinely solving the founding problem, the extraction is a legitimate cost of coordination. If beneficiary-serving at payer expense, the constraint is extractive even if it coordinates some functions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_capture_asymmetry, conceptual, 'Whether the reading''s coordination and extraction components are structurally separable or intrinsic to each other.').

omega_variable(
    mandatrophy_foundation_decay,
    'Is the ''spirit of the Council'' reading still necessary for the Church''s institutional health, or has it become an institutional inertia maintained by beneficiary constituencies with no founding problem to solve?',
    'Institutional diagnosis post-2024: if the Church exhibits renewed vitality from continuous ''spirit-driven'' reform, the founding problem remains live. If the Church faces new crises (credibility loss on sexual abuse, doctrinal incoherence, lay alienation in different directions) that the reading cannot address, the founding problem is dead and mandatrophy is present.',
    'If mandatrophy is confirmed, the constraint should be reclassified toward Piton (institutional performance maintaining extraction without solving genuine coordination problems). If the founding problem remains live, tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_foundation_decay, empirical, 'Whether the founding problem of pre-conciliar rigidity persists as a live crisis or has been replaced by new institutional challenges.').

omega_variable(
    traditionalist_suppression_internalization,
    'To what extent have traditionalist clergy internalized the reading that they embody obsolete rigidity, vs. maintaining structural rejection of it?',
    'Post-exit analysis: if traditionalist clergy who leave the priesthood or join traditionalist communities report persistent self-doubt about their tradition''s validity, suppression is substantially internalized. If they maintain confidence in their formation and community, suppression is primarily structural.',
    'If internalized, the effective suppression is higher than the structural measure (0.45) because the target carries the suppression away from the constraint; the target''s reconstruction of identity after exit would be harder. If structural, the suppression is more bounded — targets could reconstruct rapidly if constraints were removed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(traditionalist_suppression_internalization, empirical, 'Structural vs. internalized mechanisms of suppression on traditionalist clergy.').

omega_variable(
    reading_foreclosure_vs_coexistence,
    'Does the rupture-progressive reading logically foreclose the continuity reading and traditionalist reading, or do all three remain live options that happen to have unequal institutional power?',
    'Logical analysis: if a party can coherently hold both ''Vatican II as break'' and ''Vatican II as development'' by distinguishing levels of analysis (break in form, continuity in substance; continuity in doctrine, rupture in methodology), then foreclosure does not apply. If the readings assert direct contradictions (e.g., ''religious freedom is reversal of Syllabus'' vs. ''religious freedom is explication of prior teaching''), determine whether the contradiction is logical or merely empirical.',
    'If foreclosure applies, the progressive reading''s dominance reflects logical coherence. If coexistence applies, the dominance reflects power asymmetry and institutional gatekeeping, which supports snare classification. If foreclosure is partial (logical contradiction on some claims but not others), the relation is nuanced: coexists_with on some axes, forecloses on others.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, conceptual, 'Whether the readings logically exclude each other or remain logically coexistent despite unequal institutional power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__rupture_progressive_reading, 1962, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1962, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 1962, 0.25).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 1975, 0.4).
narrative_ontology:measurement(vati_tr_t1988, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 1988, 0.48).
narrative_ontology:measurement(vati_tr_t2001, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 2001, 0.52).
narrative_ontology:measurement(vati_tr_t2013, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 2013, 0.54).
narrative_ontology:measurement(vati_tr_t2024, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 2024, 0.52).

% Extraction over time
narrative_ontology:measurement(vati_be_t1962, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 1962, 0.15).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 1975, 0.38).
narrative_ontology:measurement(vati_be_t1988, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 1988, 0.54).
narrative_ontology:measurement(vati_be_t2001, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 2001, 0.62).
narrative_ontology:measurement(vati_be_t2013, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 2013, 0.68).
narrative_ontology:measurement(vati_be_t2024, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1962, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 1962, 0.15).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 1975, 0.28).
narrative_ontology:measurement(vati_su_t1988, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 1988, 0.38).
narrative_ontology:measurement(vati_su_t2001, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 2001, 0.42).
narrative_ontology:measurement(vati_su_t2013, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 2013, 0.45).
narrative_ontology:measurement(vati_su_t2024, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 2024, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__rupture_progressive_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.12).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__rupture_traditionalist_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% Vatican II doctrinal authority is a contested kernel instantiated by four distinct constraint stories. The rupture-progressive reading (this story) reads the Council as necessary break and authorizes ongoing reform; the continuity reading reads organic development within unchanging tradition; the rupture-traditionalist reading holds documents contain errors enabling heterodoxy; the composite_overdetermination reading treats Vatican II as convergence of distinct structural changes. Each reading has distinct beneficiary/victim structure, extractiveness, and enforcement mechanisms. The readings coexist in institutional tension but are linked via network as manifestations of the same kernel contest. The progressive reading (this constraint) influences the traditionalist reading by constraining its institutional possibilities (a traditionalist reading that gains dominance would transform the constraint landscape by redefining what counts as faithful interpretation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_doctrinal_authority__rupture_progressive_reading, powerless, 0.8).
constraint_indexing:directionality_override(vatican_ii_doctrinal_authority__rupture_progressive_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
