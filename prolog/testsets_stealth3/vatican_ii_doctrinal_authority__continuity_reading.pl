% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_doctrinal_authority__continuity_reading, []).

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
 *   constraint_id: vatican_ii_doctrinal_authority__continuity_reading
 *   human_readable: Hermeneutic of Continuity — Conciliar Reception Discipline
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   After the Second Vatican Council closed in 1965, the Catholic magisterium
 *   required that the Council be received as organic development within an
 *   unchanging tradition: apparent novelties (religious liberty, ecumenical
 *   engagement, liturgical reform, collegiality) are explications of implicit
 *   prior teaching, ambiguities are prudential adaptations rather than
 *   doctrinal shifts, and post-conciliar disorders are implementation errors
 *   rather than conciliar intent. This interpretive requirement is actively
 *   enforced — dicasterial assessments, mandatory profession formulas,
 *   canonical penalties, and the permission regime around the older
 *   liturgical forms — and it distributes costs unevenly: communities
 *   attached to the pre-conciliar forms bore dispossession while being
 *   officially told nothing essential changed, and theologians whose archival
 *   work documents discontinuity work under censure risk. This file is one
 *   reading of a contested kernel (see kernel_context); the family
 *   decomposition follows the epsilon-invariance principle — each sibling
 *   reading authors its own epsilon over the same standing arrangement, and
 *   the sibling stories are linked through network.affects_constraints. The
 *   claimed type and the metrics are independent authored facts: the claim
 *   states tangled_rope from the structure (a genuine coordination function
 *   joined to asymmetric enforced costs); the metrics describe observed
 *   operation without being tuned to the claim.
 *
 * KEY AGENTS:
 *   - roman_magisterium: Agenda-setter and principal collecting seat (institutional power, arbitrage exit, universal scope) — defines authentic reception through documents and dicasterial acts; the continuity claim it enforces is the same claim that shields its authority from the charge of self-contradiction.
 *   - diocesan_bishops: Secondary beneficiary (institutional/constrained/regional) — implements the settlement locally; the continuity framing lowers their enforcement costs and makes public adoption of rival readings career-ending.
 *   - reformist_clergy_and_movements: Beneficiary (organized/constrained/global) — received modernization packaged as continuity; continued access holds only short of the textual ceiling.
 *   - traditionalist_communities: Primary paying seat (organized/generational/identity_locked/global) — bears liturgical dispossession, canonical irregularity, and the epistemic injury of being told nothing essential changed; exit is fused with the identity they hold.
 *   - academic_theologians: Paying seat (moderate/constrained/continental) — career-dependent on ecclesiastical approval; research agendas bend toward confirmable continuity.
 *   - ordinary_lay_faithful: Dual seat, payer with incidental benefit (powerless/constrained/global) — absorbed the transition costs with no channel to contest the official account.
 *   - church_historians: Analytical observer (moderate/analytical/continental) — archive access reveals the full drafting structure; collects nothing, bears little.
 *   - eastern_orthodox_churches: Excluded institutional party (institutional/mobile/continental) — their rupture-shaped reading of Latin development is standing counter-testimony from outside the conversation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__continuity_reading, 0.46).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__continuity_reading, 0.66).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__continuity_reading, 0.37).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, extractiveness, 0.46).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 0.37).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__continuity_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__continuity_reading, "Hermeneutic of Continuity — Conciliar Reception Discipline").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__continuity_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__continuity_reading, 'b48e9560-e85d-4531-a619-db41566b390d').
narrative_ontology:cs_kernel_codification('b48e9560-e85d-4531-a619-db41566b390d', fixed_text).
narrative_ontology:cs_authority_grounding('b48e9560-e85d-4531-a619-db41566b390d', extraction).
narrative_ontology:cs_interpretation_layer_present('b48e9560-e85d-4531-a619-db41566b390d').
narrative_ontology:cs_reading_relation('b48e9560-e85d-4531-a619-db41566b390d', vatican_ii_doctrinal_authority__rupture_traditionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('b48e9560-e85d-4531-a619-db41566b390d', vatican_ii_doctrinal_authority__rupture_progressive_reading, influences).
narrative_ontology:cs_reading_relation('b48e9560-e85d-4531-a619-db41566b390d', vatican_ii_doctrinal_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('b48e9560-e85d-4531-a619-db41566b390d', foundational, indefectibility_guarantees_continuity).
narrative_ontology:cs_axiom_status(indefectibility_guarantees_continuity, holdable).
narrative_ontology:cs_axiom_grounding('b48e9560-e85d-4531-a619-db41566b390d', indefectibility_guarantees_continuity, theological).
narrative_ontology:cs_axiom('b48e9560-e85d-4531-a619-db41566b390d', foundational, apparent_novelties_explicate_prior_teaching).
narrative_ontology:cs_axiom_status(apparent_novelties_explicate_prior_teaching, holdable).
narrative_ontology:cs_axiom_grounding('b48e9560-e85d-4531-a619-db41566b390d', apparent_novelties_explicate_prior_teaching, empirically_contingent).
narrative_ontology:cs_reference_frame('b48e9560-e85d-4531-a619-db41566b390d', organic_development_of_unchanged_deposit).
narrative_ontology:cs_drift_state('b48e9560-e85d-4531-a619-db41566b390d', contemporary_synodal_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('b48e9560-e85d-4531-a619-db41566b390d', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, roman_magisterium).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, diocesan_bishops).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, reformist_clergy_and_movements).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__continuity_reading, traditionalist_communities).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__continuity_reading, academic_theologians).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__continuity_reading, ordinary_lay_faithful).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, ordinary_lay_faithful).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__continuity_reading, newman_development_of_doctrine).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__continuity_reading, vincentian_canon).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__continuity_reading, hermeneutic_of_reform_in_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines what counts as authentic reception of the Council: issues interpretive documents, dicasterial assessments, and disciplinary acts; commissions catechetical and liturgical implementation. The claim it enforces — that the Church cannot contradict herself — is the same claim that shields its own teaching authority from the charge of having reversed course, so administering the interpretive standard returns legitimacy to the administering office. It bears credibility costs each time a reconciliation strains, but it controls the interpretive instruments and can reframe ambiguities by issuing new documents.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, roman_magisterium, agenda_setter,
    institutional, generational, arbitrage, universal).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__continuity_reading, roman_magisterium, beneficiary).

% Implement the conciliar settlement in their dioceses under the continuity banner. The framing lowers their day-to-day costs: resistance can be treated as disobedience rather than warranted alarm, and hard questions about what actually changed can be referred upward. A bishop who publicly adopts a rival reading of the Council forfeits career prospects and invites intervention; staying inside the frame is rewarded with ordinary governance.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, diocesan_bishops, beneficiary,
    institutional, biographical, constrained, regional).

% Priests, orders, and new ecclesial movements that wanted modernization received it packaged as continuity: they gained the reforms without carrying the accusation of infidelity or the schism risk. Their continued standing depends on not pressing past the textual ceiling; those who push further come under the same disciplinary attention that guards the frame.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, reformist_clergy_and_movements, beneficiary,
    organized, biographical, constrained, global).

% Communities attached to the pre-conciliar liturgical and doctrinal forms. Across the interval they lost access to their liturgical patrimony for decades, lived under canonical irregularity after 1988, and saw restrictions return in 2021 after a brief liberalization. Alongside the material losses sits the epistemic injury of an official account that says nothing essential changed — which recasts their attachment as nostalgia and their objection as disobedience. Leaving full communion would mean becoming the schism they understand themselves to be preventing, so most stay and absorb the costs.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, traditionalist_communities, payer,
    organized, generational, identity_locked, global).

% University and seminary scholars testing the continuity claim against the documentary record. Employment, mandata, and publication channels run through ecclesiastical approval; work that documents discontinuity attracts investigation or quiet shelving, so research agendas bend toward conclusions the frame can certify. Most pay in self-censorship; a minority have paid in formal censure.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, academic_theologians, payer,
    moderate, biographical, constrained, continental).

% Received the settlement as implemented: new liturgical forms presented as the same faith in a new idiom, catechesis asserting continuity. They keep doctrinal stability and membership in a worldwide communion; they paid the transition — abandoned devotions, dismantled parish cultures, disrupted musical and liturgical inheritance — with no channel to contest the official account except departure, which the continuity framing itself discredits.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, ordinary_lay_faithful, payer,
    powerless, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__continuity_reading, ordinary_lay_faithful, beneficiary).

% Archive-based scholars of the Council itself: draft redactions, diaries, floor speeches, correspondence. Their materials reveal which passages were fought over, which ambiguities were inserted deliberately, and where the smooth published narrative compresses real conflict. They collect nothing from the arrangement and bear little direct cost; their findings circulate mainly in academic channels outside the disciplinary perimeter.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, church_historians, observer,
    moderate, biographical, analytical, continental).

% Sister communions outside the Latin interpretive conversation. Their own experience of Latin doctrinal development — the nineteenth-century papal definitions — reads to them as rupture, and their non-reception of that model is standing testimony against the premise that development is always benign explication. They would contest the continuity account's premises if admitted to the conversation, but they sit structurally outside it.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, eastern_orthodox_churches, excluded,
    institutional, civilizational, mobile, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_doctrinal_authority__continuity_reading, roman_magisterium).
narrative_ontology:fixing_cost_class(vatican_ii_doctrinal_authority__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single authoritative hermeneutic for reading the Council across a global communion: one account of what changed and what did not, so that clergy formation, catechesis, liturgical practice, and ecumenical dialogue proceed from a shared interpretive baseline instead of fragmenting into incompatible private readings.
% TRANSFER_FUNCTION: Moves interpretive authority upward — from local communities, theologians, and historians to the central magisterium, which alone certifies what the conciliar texts mean — and moves the costs of the liturgical and pastoral transition onto those attached to the pre-conciliar forms, while the legitimacy gains of change-without-rupture accrue to the center and to implementing bishops.
% ABSENT_VOICES: Church historians with access to the drafting archives sit outside the magisterial conversation that fixes the official account; the Eastern Orthodox churches, whose own reading of Latin development is rupture-shaped, are structurally outside; rank-and-file laity have no channel to contest the interpretive standard short of departure, which the continuity framing itself discredits as nostalgia.
% DISAPPEARANCE_RATIONALE: Overnight removal would unbundle reception immediately: traditionalist communities would treat the event as vindication of their reading, progressive theologians would invoke the Council beyond its texts, and the magisterium would face the raw contradiction charge the interpretive discipline currently absorbs — seminary curricula, liturgical regulation, and ecumenical agreements negotiated on the continuity premise would all require renegotiation.
% FOUNDING_PROBLEM: The Council issued texts on religious liberty, ecumenism, liturgy, and collegiality that appeared to contradict earlier magisterial acts condemning the same positions; receiving both bodies of teaching as authoritative required an interpretive rule that could reconcile them without conceding that the Church had taught error.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the traditionalist movement's entire institutional existence attests the tension remains unresolved; academic histories of the Council (draft-stage diaries and redaction studies) document deliberate ambiguities inserted to hold the documents together; Orthodox non-reception of the Latin development model stands as external counter-testimony. No party outside the dispute attests that the reconciliation is settled.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_doctrinal_authority__continuity_reading, 0.46, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__continuity_reading_tests).
:- end_tests(vatican_ii_doctrinal_authority__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.46 at interval end) because the arrangement pairs a real coordination good — one shared hermeneutic holding a global communion together across a visibly discontinuous-looking reform — with concentrated, asymmetric costs: liturgical dispossession and canonical irregularity for traditionally attached communities, career discipline for theologians whose findings outrun the official account. Suppression (0.66) is authored as a raw structural property and is deliberately NOT scaled by power or scope in this story — the engine applies its own context scaling to extractiveness only; the value reflects the actual enforcement machinery: dicasterial investigations, mandatory profession formulas, canonical penalties, and the permission regime governing the older liturgical forms. Theater (0.37) captures the growing share of reception activity that is ritualized affirmation — prefatory assertions that nothing contradicts prior teaching, anniversary commemorations rehearsing seamlessness — as against live interpretive work. Accessibility collapse (0.60): inside the canonical perimeter alternatives largely collapse (a bishop or seminary professor cannot hold a rupture reading and keep office), but alternatives persist at the margins — irregular communities, secular academies, the Orthodox communion — so collapse is partial, not total. Resistance (0.55): sustained and organized on two flanks simultaneously, traditionalist and progressive, which is itself structurally significant — the two resisting camps oppose each other as much as the arrangement, making a cross-cutting coalition against the interpretive discipline improbable and keeping enforcement costs manageable. The temporal series run on one shared grid (1965-2025 at decade points) and show a cyclical enforcement pattern rather than monotonic drift: tightening through the censure-heavy 1970s-1990s, relaxation after the 2007 liberalization of the older liturgical forms, re-tightening after the 2021 restrictions. The oscillation is not noise: the 2007 relaxation raised traditionalist attachment and expectation, which made the 2021 reversal costlier and deepened dependence on central favor — an intermittent-reinforcement dynamic in which the cycle itself disciplines the paying seat.
 *
 * PERSPECTIVAL GAP:
 *   The seats should classify differently and the divergence is the finding. From the magisterium's seat the arrangement looks like a coordination instrument it built, administers, and can reframe at will — its arbitrage-grade exit (issue a new document, redefine a term, commission a new synthesis) means no fixed structure presses on it. From the traditionalist seat the same arrangement operates as enforced dispossession with the injury compounded: the official account denies that what they lost was lost, converting their grief into alleged disobedience. Bishops and reformist clergy sit between — beneficiaries whose continuing benefit is conditional on staying inside the frame. Same-level divergence appears between the magisterium and the bishops: nominally both institutional, but the center's exit is arbitrage while a bishop's is constrained (office effectively forfeited by public defection from the frame), so identical nominal standing yields different experienced arrangements. The identity-lock mechanism on the traditionalist seat is ideological-relational fusion: the community's self-concept is the faithful remnant preserving what the authorities abandoned, so exit equals becoming the schism they exist to prevent; if a durable regularized status ever dissolved that frame, the seat's effective position would soften materially. Suppression splits roughly 60% structural (canonical penalties, career gates, liturgical permissions) and 40% internalized (obedience formation, fused identity), which is why the exit-block omega is empirical rather than rhetorical.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. The magisterium declares as beneficiary and holds arbitrage exit, placing it near the full-beneficiary end; the credibility costs it bears on each strained reconciliation keep it off the extreme. Bishops and reformist clergy/movements are beneficiaries with constrained exit — subsidized but not sovereign. Traditionalist communities declare as victims with identity_locked exit, which pins them near the full-target end: trapped or fused targets sit nearer full-target than mobile ones. Academic theologians are victims with constrained exit — high but not maximal. Ordinary laity carry both declarations (payer with incidental benefit), landing near symmetric. The excluded and observer seats stand outside the transfer: the Orthodox churches are mobile (already outside the arrangement's jurisdiction) and the historians analytical. No directionality overrides are needed — the structural declarations plus exit atoms produce the intended spread without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling apparently contradictory magisterial acts — is still live, so this is not a resolved-mandatrophy case: the arrangement has not outlived its function. The classification discipline matters in both directions here. Reading the arrangement as pure coordination would erase the identifiable payers — communities that lost their liturgical patrimony for decades and scholars censured for documenting discontinuity — and launder enforced costs as communal prudence. Reading it as pure extraction would erase the genuine coordination function: a billion-member communion with no shared hermeneutic fragments into incompatible private readings, a real collective-action failure the arrangement mitigates. The tangled-rope classification holds both facts. One subsystem trends toward atrophy: the ritualized continuity-affirmation layer (anniversary rhetoric, prefatory assertions) is increasingly performance stacked on a live enforcement core — visible in the rising theater ratio — but the enforcement core itself remains functional, so the whole does not yet meet the inertial profile.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading (continuity_reading) of the kernel vatican_ii_doctrinal_authority; which reading governs reception determines the entire victim and beneficiary structure — what evidence would establish which reading the operative arrangement actually instantiates?',
    'Comparative classification across the four sibling stories plus reception-behavior evidence: which reading''s vocabulary dominates curial documents, seminary curricula, and disciplinary acts over time.',
    'If a rupture reading becomes operative, the continuity constraint''s coordination function collapses and its payers re-sort (traditionalist communities become beneficiaries under the rupture-traditionalist frame; progressives become its targets); epsilon re-indexes accordingly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Kernel-level contest: which reading of Vatican II''s authority is operative.').

omega_variable(
    development_vs_authority_protection,
    'Is the continuity requirement a genuine structural feature of how dogmatic development works (the Vincentian and Newman logic that true development explicates prior teaching), or a constructed arrangement whose primary function is protecting magisterial authority from the charge of self-contradiction?',
    'Test cases where development ran against institutional interest: if the apparatus reliably certifies developments that cost the center authority (as it eventually did with religious liberty), the developmental logic is real; if certification tracks institutional interest, protection dominates.',
    'Genuine developmental logic supports the coordination side of the hybrid; protection dominance pushes the structure toward pure extraction with the coordination story as cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(development_vs_authority_protection, conceptual, 'Whether the arrangement''s coordination function is developmental logic or authority protection.').

omega_variable(
    traditionalist_exit_block_mechanism,
    'Is the traditionalist communities'' inability to exit structural (canonical irregularity, loss of sacramental access, property ties) or internalized (identity fusion — leaving equals becoming the schism they exist to prevent)?',
    'Post-departure trajectories: compare communities that accepted regularization with those that left; whether former members report persistent identity conflict after exit.',
    'If internalized, effective suppression exceeds the structural measure — the arrangement travels with the agent after exit; the traditionalist seat''s computed position hardens toward full-target.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(traditionalist_exit_block_mechanism, empirical, 'Structural versus internalized source of the traditionalist exit block.').

omega_variable(
    extraction_domain_asymmetry,
    'The continuity reading''s costs concentrate differently by domain — doctrinal change is certified as costless (nothing new) while liturgical and pastoral practice carries heavy compliance and dispossession costs. Are these one arrangement with a skewed cost profile, or two separable arrangements (a doctrinal-interpretive one and a liturgical-practical one) that the unified reception label merges?',
    'Observe whether the two domains decouple under stress: jurisdictions or orders where liturgical practice reverted during the 2007-2021 liberalization window while doctrinal interpretation stayed continuous — if the enforcement regimes move independently, the arrangements are separable and should be decomposed.',
    'If separable, this story''s blended epsilon understates the liturgical-practical arrangement''s costs and overstates the doctrinal one; decomposition would yield a low-cost doctrinal story and a high-cost liturgical story linked by network edges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_domain_asymmetry, empirical, 'Whether doctrinal and liturgical cost profiles are one constraint or two.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__continuity_reading, 1965, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1965, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 1965, 0.18).
narrative_ontology:measurement_basis(vati_tr_t1965, observed).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 1975, 0.23).
narrative_ontology:measurement_basis(vati_tr_t1975, observed).
narrative_ontology:measurement(vati_tr_t1985, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 1985, 0.27).
narrative_ontology:measurement_basis(vati_tr_t1985, observed).
narrative_ontology:measurement(vati_tr_t1995, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 1995, 0.31).
narrative_ontology:measurement_basis(vati_tr_t1995, observed).
narrative_ontology:measurement(vati_tr_t2005, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 2005, 0.35).
narrative_ontology:measurement_basis(vati_tr_t2005, observed).
narrative_ontology:measurement(vati_tr_t2015, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 2015, 0.33).
narrative_ontology:measurement_basis(vati_tr_t2015, observed).
narrative_ontology:measurement(vati_tr_t2025, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 2025, 0.37).
narrative_ontology:measurement_basis(vati_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(vati_be_t1965, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 1965, 0.26).
narrative_ontology:measurement_basis(vati_be_t1965, observed).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 1975, 0.33).
narrative_ontology:measurement_basis(vati_be_t1975, observed).
narrative_ontology:measurement(vati_be_t1985, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 1985, 0.41).
narrative_ontology:measurement_basis(vati_be_t1985, observed).
narrative_ontology:measurement(vati_be_t1995, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 1995, 0.45).
narrative_ontology:measurement_basis(vati_be_t1995, observed).
narrative_ontology:measurement(vati_be_t2005, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 2005, 0.42).
narrative_ontology:measurement_basis(vati_be_t2005, observed).
narrative_ontology:measurement(vati_be_t2015, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 2015, 0.4).
narrative_ontology:measurement_basis(vati_be_t2015, observed).
narrative_ontology:measurement(vati_be_t2025, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 2025, 0.46).
narrative_ontology:measurement_basis(vati_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1965, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 1965, 0.42).
narrative_ontology:measurement_basis(vati_su_t1965, observed).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 1975, 0.52).
narrative_ontology:measurement_basis(vati_su_t1975, observed).
narrative_ontology:measurement(vati_su_t1985, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 1985, 0.64).
narrative_ontology:measurement_basis(vati_su_t1985, observed).
narrative_ontology:measurement(vati_su_t1995, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 1995, 0.7).
narrative_ontology:measurement_basis(vati_su_t1995, observed).
narrative_ontology:measurement(vati_su_t2005, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 2005, 0.6).
narrative_ontology:measurement_basis(vati_su_t2005, observed).
narrative_ontology:measurement(vati_su_t2015, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 2015, 0.56).
narrative_ontology:measurement_basis(vati_su_t2015, observed).
narrative_ontology:measurement(vati_su_t2025, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 2025, 0.66).
narrative_ontology:measurement_basis(vati_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_doctrinal_authority__rupture_progressive_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_doctrinal_authority__rupture_traditionalist_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_doctrinal_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% Family decomposition of the kernel vatican_ii_doctrinal_authority into four readings: continuity_reading (this file), rupture_progressive_reading, rupture_traditionalist_reading, composite_overdetermination_reading. The colloquial label 'what Vatican II was' conflates four structurally distinct constraints with different epsilon, different victim sets, and different enforcement logics; each is authored separately and linked here. Upstream/downstream: the continuity reading is the officially articulated frame and therefore shapes the operating environment of the siblings (its 2005 explicit articulation delegitimized the progressive reading's spirit-of-the-Council warrant), while the traditionalist reading's persistence continuously pressures the continuity frame's credibility.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
