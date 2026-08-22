% ============================================================================
% CONSTRAINT STORY: decalogue_image_prohibition__moderate_iconoclast_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_decalogue_image_prohibition__moderate_iconoclast_reading, []).

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
 *   constraint_id: decalogue_image_prohibition__moderate_iconoclast_reading
 *   human_readable: Two-Tier Image Law: Statuary Prohibited, Flat Imagery Under Permanent Review (Moderate Iconoclast Reading)
 *   domain: theology/religious authority/visual culture
 *
 * SUMMARY:
 *   A religious authority administers a two-tier image law: freestanding
 *   three-dimensional statuary is prohibited outright as carrying the highest
 *   idolatry risk, while two-dimensional religious images are permitted only
 *   through a permanent apparatus of chartering, design review, placement
 *   rules, inspection, and sanction. The regime settles a question the
 *   community repeatedly failed to settle by riot and ad-hoc ruling, and it
 *   does so through a structure that simultaneously delivers authorized
 *   devotional imagery and collects fees, fines, forfeitures, and
 *   adjudication prestige for the authority. This file instantiates ONE
 *   reading of the decalogue_image_prohibition kernel, the
 *   moderate_iconoclast_reading; the iconoclast and iconodule siblings are
 *   separate constraints with their own epsilon values and stakeholder
 *   surfaces, linked via network.affects_constraints, and are not averaged
 *   into this story. KEY AGENTS (by structural relationship): -
 *   religious_regulatory_authority: agenda-setter and primary beneficiary
 *   (institutional/identity_locked) — writes the line, staffs the review,
 *   collects fees, fines, and forfeited works - licensed_icon_painters:
 *   chartered producer-beneficiaries (organized/constrained) — protected
 *   market, paid tolls - figural_statuary_workshops: primary targets
 *   (moderate/constrained) — medium prohibited outright, stock seized -
 *   lay_devotional_confraternities: dual-positioned payers
 *   (organized/constrained) — authorized images received, votive life
 *   micromanaged - unlicensed_image_makers: full targets
 *   (powerless/constrained) — outside the charter, stock liable to public
 *   breaking - rival_interpretive_authorities: excluded interpreters
 *   (powerful/trapped) — teach broader material mediation, kept out of
 *   rule-setting - religious_history_scholars: analytical observers
 *   (analytical/analytical) — reconstruct the founding record, audit
 *   enforcement against stated aims
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decalogue_image_prohibition__moderate_iconoclast_reading, 0.58).
domain_priors:suppression_score(decalogue_image_prohibition__moderate_iconoclast_reading, 0.68).
domain_priors:theater_ratio(decalogue_image_prohibition__moderate_iconoclast_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__moderate_iconoclast_reading, tangled_rope).
narrative_ontology:human_readable(decalogue_image_prohibition__moderate_iconoclast_reading, "Two-Tier Image Law: Statuary Prohibited, Flat Imagery Under Permanent Review (Moderate Iconoclast Reading)").
narrative_ontology:topic_domain(decalogue_image_prohibition__moderate_iconoclast_reading, "theology/religious authority/visual culture").

domain_priors:requires_active_enforcement(decalogue_image_prohibition__moderate_iconoclast_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__moderate_iconoclast_reading, 'be8e7abe-e9f0-4285-b8c3-67f525294015').
narrative_ontology:cs_kernel_codification('be8e7abe-e9f0-4285-b8c3-67f525294015', fixed_text).
narrative_ontology:cs_authority_grounding('be8e7abe-e9f0-4285-b8c3-67f525294015', lineage).
narrative_ontology:cs_interpretation_layer_present('be8e7abe-e9f0-4285-b8c3-67f525294015').
narrative_ontology:cs_reading_relation('be8e7abe-e9f0-4285-b8c3-67f525294015', decalogue_image_prohibition__iconoclast_reading, forecloses).
narrative_ontology:cs_reading_relation('be8e7abe-e9f0-4285-b8c3-67f525294015', decalogue_image_prohibition__iconodule_reading, influences).
narrative_ontology:cs_axiom('be8e7abe-e9f0-4285-b8c3-67f525294015', foundational, dimensionality_grades_idolatry_risk).
narrative_ontology:cs_axiom_status(dimensionality_grades_idolatry_risk, holdable).
narrative_ontology:cs_axiom_grounding('be8e7abe-e9f0-4285-b8c3-67f525294015', dimensionality_grades_idolatry_risk, empirically_contingent).
narrative_ontology:cs_axiom('be8e7abe-e9f0-4285-b8c3-67f525294015', foundational, regulated_flat_imagery_preserves_commandment).
narrative_ontology:cs_axiom_status(regulated_flat_imagery_preserves_commandment, holdable).
narrative_ontology:cs_axiom_grounding('be8e7abe-e9f0-4285-b8c3-67f525294015', regulated_flat_imagery_preserves_commandment, instrumental).
narrative_ontology:cs_reference_frame('be8e7abe-e9f0-4285-b8c3-67f525294015', graded_idolatry_risk_framework).
narrative_ontology:cs_drift_state('be8e7abe-e9f0-4285-b8c3-67f525294015', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('be8e7abe-e9f0-4285-b8c3-67f525294015', '').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__moderate_iconoclast_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__moderate_iconoclast_reading, religious_regulatory_authority).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__moderate_iconoclast_reading, licensed_icon_painters).
narrative_ontology:constraint_victim(decalogue_image_prohibition__moderate_iconoclast_reading, figural_statuary_workshops).
narrative_ontology:constraint_victim(decalogue_image_prohibition__moderate_iconoclast_reading, unlicensed_image_makers).
narrative_ontology:constraint_victim(decalogue_image_prohibition__moderate_iconoclast_reading, lay_devotional_confraternities).
narrative_ontology:constraint_victim(decalogue_image_prohibition__moderate_iconoclast_reading, rival_interpretive_authorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__moderate_iconoclast_reading, lay_devotional_confraternities).
narrative_ontology:constraint_victim(decalogue_image_prohibition__moderate_iconoclast_reading, licensed_icon_painters).
narrative_ontology:constraint_vindicates(decalogue_image_prohibition__moderate_iconoclast_reading, second_commandment_literalist_hermeneutics).
narrative_ontology:constraint_vindicates(decalogue_image_prohibition__moderate_iconoclast_reading, graded_idolatry_risk_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the boundary between prohibited and permitted imagery, charters and inspects licensed workshops, adjudicates disputed cases, and sanctions violations through confiscation, breaking of works, fines, and public censure. Collects licensing fees, fines, and forfeited property, and accrues adjudication prestige from every settled dispute. Its legitimacy is fused with its gatekeeping office: liberalizing the regime would concede that rival interpreters were right, so it cannot stand down from enforcement without dissolving its own standing.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, religious_regulatory_authority, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(decalogue_image_prohibition__moderate_iconoclast_reading, religious_regulatory_authority, beneficiary).

% Produce authorized two-dimensional devotional images under charter. The exclusion of unlicensed competitors protects their market, and charter status confers standing with patrons. They pay application fees, submit designs for approval, absorb inspection interruptions, and rework commissions the reviewers reject. Leaving the licensed channel means losing access to devotional commissions, their principal market.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, licensed_icon_painters, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(decalogue_image_prohibition__moderate_iconoclast_reading, licensed_icon_painters, payer).

% Master carvers whose medium, freestanding three-dimensional figures, is prohibited outright. Commissions disappear, finished stock is liable to seizure and public breaking, and apprentices defect to the permitted flat-image trades. Adaptation means retraining into relief carving or architectural ornament under license; refusal means the workshop closes.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, figural_statuary_workshops, payer,
    moderate, biographical, constrained, regional).

% Organized lay groups that commission and process with sacred images. They receive authorized icons and prints for devotion, a good they genuinely want, but carry the regime's uncertainties: votive objects confiscated when a reviewer judges them too sculptural, processions rerouted around unapproved imagery, and commission costs inflated by the fees their contracted works must carry. They hold no seat where the rules are made.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, lay_devotional_confraternities, payer,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(decalogue_image_prohibition__moderate_iconoclast_reading, lay_devotional_confraternities, beneficiary).

% Folk artisans painting ex-votos, household images, and festival banners outside the charter system. Their work is cheap and woven into local practice; enforcement reaches them as confiscation and occasional public breaking of stock. Entering the licensed channel means fees they cannot carry; the secular market for their skills is thin.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, unlicensed_image_makers, payer,
    powerless, immediate, constrained, local).

% Preachers, mystics, and renegade jurists teaching that material mediation is more broadly legitimate than the regime allows. Their followings grow fastest where licensed supply feels grudging. Censure, exile, and the licensing monopoly keep them out of the councils that write the rules; their message requires the very community they are exiled from, so they cannot simply depart and keep their audience.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, rival_interpretive_authorities, excluded,
    powerful, generational, trapped, continental).

% Compare image regimes across traditions and centuries, reconstruct the founding controversies from conciliar acta and town chronicles, and audit enforcement records against the regime's stated aims. They hold no office in the apparatus and can therefore say what the seated parties cannot.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, religious_history_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(decalogue_image_prohibition__moderate_iconoclast_reading, religious_regulatory_authority).
narrative_ontology:fixing_cost_class(decalogue_image_prohibition__moderate_iconoclast_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Settles, for a community committed to the second commandment, the question the community could not otherwise settle: where the line against idolatry falls. The regime fixes that line at medium dimensionality (no freestanding figures) and operates a permanent review channel through which permissible flat religious images are approved, placed, and monitored.
% TRANSFER_FUNCTION: Moves charter fees, fines, and forfeited property from image makers and lay devotees to the regulatory authority; moves the decision over what may be displayed from artists and patrons to the authority's reviewers; moves market protection and standing to chartered producers at the expense of unlicensed ones.
% ABSENT_VOICES: Rival interpretive authorities are censured out of the councils; lay confraternities send no delegates; working sculptors, the class the harshest rule falls on, have no advocate in the review chambers. The unanimity of the rule-making record reflects who was admitted to the room.
% DISAPPEARANCE_RATIONALE: Chartered monopolies lapse; confiscated statuary revives in some regions while others adopt stricter full bans; devotional supply diversifies; the authority loses fee income, forfeiture revenue, and adjudication prestige. Neighboring communities would sort among the rival answers to the image question rather than converge on this one.
% FOUNDING_PROBLEM: Communities bound to the second commandment faced an unresolved conflict between the letter of the prohibition and the educational and devotional work images plainly did: recurring idolatry scares around popular images, factional riots between stricter and laxer parties, and ad-hoc rulings that varied town by town. The two-tier regime was built to fix the line (no freestanding figures, flat images under review) and to staff the line with a permanent reviewing body.
% FOUNDING_PROBLEM_CORROBORATION: Conciliar acta and town chronicles from the founding decades, written before the licensing apparatus existed, attest the riots and scares. Modern historians of religion, holding no office in the regime, corroborate the founding crisis while dating its acute phase generations back. Full-prohibition partisans corroborate that the original problem was real while denying this reading's solution; iconodule partisans dispute that the solution ever matched the problem. No seat outside the benefiting parties attests that the acute founding emergency persists today.
narrative_ontology:disappearance_verdict(decalogue_image_prohibition__moderate_iconoclast_reading, world_rearranges).
narrative_ontology:founding_problem_status(decalogue_image_prohibition__moderate_iconoclast_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(decalogue_image_prohibition__moderate_iconoclast_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(decalogue_image_prohibition__moderate_iconoclast_reading, 'none', 1).
narrative_ontology:epsilon_provenance(decalogue_image_prohibition__moderate_iconoclast_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(decalogue_image_prohibition__moderate_iconoclast_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(decalogue_image_prohibition__moderate_iconoclast_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(decalogue_image_prohibition__moderate_iconoclast_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.58: the regime transfers real value (charter fees, review delays, forced reworks, confiscated stock, a stranded sculptural trade) while delivering a permission structure its participants demonstrably value relative to the stricter alternative; substantial, short of pure extraction. Suppression 0.68: persistence rests on active machinery (chartering, inspection, confiscation, censure of rival teachers) rather than voluntary assent; the permitted flat-image channel blunts but does not remove the coercive edge. Theater 0.35: boundary screening and adjudication do real work, but a large share of activity is vigilance performance (routine inspection of long-approved workshops, ceremonial breakings, condemnations aimed at already-anxious compliant producers). Accessibility collapse 0.45: alternatives stay visible and live (fuller prohibition, underground veneration, covert production, exit from the tradition); none is extinguished. Resistance 0.55: recurring (patron pushback, artisan evasion, resurgences of rival teaching, each answered by retightening). Claim/metric independence: the manifest seeded this story with a snare hypothesis; on the structural data I refine the claim to tangled_rope, because the regime possesses a genuine coordination function (it settles, durably, a line the community could not otherwise settle) AND asymmetric extraction through the same structure (gatekeeping receipts accruing to the authority), while every metric above is authored from the regime's observed operation and tuned to neither label. Suppression is authored as a raw structural property; only extractiveness is scaled by the engine (by directionality and by the continental scope, whose long verification chains amplify chi for trapped targets). Cyclical pattern: the shared-grid series traces roughly 80-year enforcement cycles: crackdown (T0, T80), relaxation and procedural drift (T40, T140), scare-driven retightening (T60, T160 rising). The oscillation is itself an extraction mechanism: leniency phases deepen reliance on permitted imagery, and each crackdown converts that reliance into preemptive licensing, fee payment, and design submission (intermittent reinforcement). Endpoint values reflect the post-settlement phase of the latest cycle.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from one structure. From the authority's chair the regime is faithful stewardship: a commanded boundary, honestly administered, with review protecting the flock from relapse. From the statuary workshops the same structure is the destruction of a trade by fiat. From licensed painters it is a mixed blessing: a moat around their market that they also pay tolls to cross. From the confraternities it is protection experienced as paternalism: real images delivered, votive life micromanaged. The authority's exit is identity_locked in the institutional sense: the organization has become its gatekeeping function, so liberalization reads as self-dissolution and as admission that the rival teachers were right; this fusion, not external coercion alone, holds the enforcer in place, and if the identity frame broke (a leadership generation willing to concede the line was prudential rather than commanded), the enforcement economy would unravel quickly. The engine derives these divergent per-seat classifications from the structural data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations pull d toward 0: the authority sits near the full-beneficiary end (it collects fees, fines, forfeitures, and adjudication prestige and bears none of the compliance burden); licensed painters derive low-moderate d (declared beneficiaries, constrained exit) though their true position is dual, nearer 0.45, protected market against paid tolls. Victim declarations pull d toward 1: statuary workshops and unlicensed makers sit near the full-target end (property seized, trades barred, exit thin); confraternities derive target-leaning d (declared victims, constrained exit) though they also receive authorized images; rival interpretive authorities are declared victims whose exclusion is the enforcement object itself, and their trapped exit pushes them toward the target end. I author no directionality_overrides: the derivation already separates the two 'organized' seats (painters via beneficiary declaration, confraternities via victim declaration), and a power-atom-keyed override at 'organized' would drag the confraternities away from their correctly derived target-leaning value to fix a smaller error at the painters. Scope: the regime runs at continental scale across dispersed communities, so compliance verification is hard and the engine's scope amplification lands hardest on the trapped and powerless seats.
 *
 * MANDATROPHY ANALYSIS:
 *   Tangled_rope holds two truths together that single-label readings flatten. Calling the regime a snare erases the genuine service: it does settle, durably, a line the community repeatedly failed to settle by riot and improvised ruling. Calling it a rope erases the receipts: fees, forfeitures, a monopolized review channel, and a rival class kept out of the room. The mandatrophy question is live rather than settled: the founding problem's acute phase (riots, scares) is generations back, and the apparatus now renews its own mandate cyclically, each crackdown manufacturing the vigilance that justifies the next. The R5 mismatch consumer should watch status=contested crossed with verdict=world_rearranges: if enforcement theatricalizes across successive cycles (trough theater_ratio drifting 0.45 to 0.47) while incident-prevention yield falls, the trajectory bends toward piton; if the coordination function hollows, because the community stops needing the line settled, the residue is snare. Coalition check: the payer classes are divided by design. Painters profit from the statuary ban that ruins the carvers; confraternities depend on the licensed channel that taxes them; the diffuse majority never coalesces, and that division is itself a product of the two-tier structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'Is the two-tier structure a faithful reading of the decalogue_image_prohibition kernel''s own logic, or a later juridical construction projected onto an ambiguous commandment?',
    'Philological and reception-history analysis of the commandment''s earliest interpretations, conducted independently of the licensing tradition''s own scholarship.',
    'If construction, this reading''s claim to transmit the kernel weakens, the constraint reclassifies toward constructed extraction, and the sibling readings gain equal textual standing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Whether the moderate reading transmits the kernel or constructs it.').

omega_variable(
    sibling_scope_predicate_location,
    'Where exactly do the three readings of the kernel disagree, and what would adopting a sibling change structurally?',
    'Locate the disputed predicate: what makes a representation prohibited (any material representation used in worship / the worship-act performed toward it / the medium''s dimensionality). Model each sibling as its own constraint and compare stakeholder surfaces.',
    'The iconoclast sibling deletes the permitted flat-image channel entirely: the licensed-painter beneficiary class vanishes and compliance costs convert into outright prohibition. The iconodule sibling deletes the prohibition core: gatekeeping converts into sacramental mediation and the victim classes dissolve. Classification shifts sharply under either adoption; this file''s numbers are valid only for the moderate reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_scope_predicate_location, conceptual, 'Structural location of the inter-reading disagreement and its classification consequences.').

omega_variable(
    graded_risk_empirical_basis,
    'Does freestanding three-dimensional statuary actually carry higher idolatry incidence than regulated two-dimensional imagery, as the graded-risk theory requires?',
    'Comparative historical data on idolatrous-practice incidence by medium across image-regulating traditions, controlling for enforcement intensity.',
    'If the risk gradient fails, the 3D/2D line loses its justification, the coordination story degrades toward cover, and the constraint trends snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(graded_risk_empirical_basis, empirical, 'Empirical standing of the dimensionality-grading premise.').

omega_variable(
    enforcement_outcome_audit,
    'What share of regulatory activity causally prevents idolatrous misuse rather than performing vigilance?',
    'Audit linking inspections, censures, and breakings to prevented incidents, using enforcement ledgers against recorded abuse episodes.',
    'A high non-effective share confirms theater drift and a piton trajectory during relaxed phases; a low share supports the genuine-function reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_outcome_audit, empirical, 'Functional versus performative share of enforcement activity.').

omega_variable(
    crackdown_cycle_endogeneity,
    'Is the crackdown-relaxation oscillation driven by idolatry incidents (exogenous) or by the authority''s fiscal and succession cycles (endogenous intermittent reinforcement)?',
    'Correlate crackdown timing with incident reports versus authority budget stress and leadership-succession calendars.',
    'If endogenous, the cycle is itself the extraction engine: effective extraction exceeds any point measurement, pre-compliance is manufactured, and the snare reading gains support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crackdown_cycle_endogeneity, empirical, 'Exogenous versus engineered character of the enforcement cycle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decalogue_image_prohibition__moderate_iconoclast_reading, 0, 160).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deca_tr_t0, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(deca_tr_t20, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(deca_tr_t40, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 40, 0.45).
narrative_ontology:measurement(deca_tr_t60, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 60, 0.33).
narrative_ontology:measurement(deca_tr_t80, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 80, 0.26).
narrative_ontology:measurement(deca_tr_t100, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 100, 0.34).
narrative_ontology:measurement(deca_tr_t120, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 120, 0.42).
narrative_ontology:measurement(deca_tr_t140, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 140, 0.47).
narrative_ontology:measurement(deca_tr_t160, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 160, 0.35).

% Extraction over time
narrative_ontology:measurement(deca_be_t0, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(deca_be_t20, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 20, 0.53).
narrative_ontology:measurement(deca_be_t40, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement(deca_be_t60, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 60, 0.56).
narrative_ontology:measurement(deca_be_t80, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 80, 0.66).
narrative_ontology:measurement(deca_be_t100, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 100, 0.63).
narrative_ontology:measurement(deca_be_t120, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 120, 0.56).
narrative_ontology:measurement(deca_be_t140, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 140, 0.51).
narrative_ontology:measurement(deca_be_t160, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 160, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(deca_su_t0, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(deca_su_t20, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(deca_su_t40, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement(deca_su_t60, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 60, 0.65).
narrative_ontology:measurement(deca_su_t80, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 80, 0.76).
narrative_ontology:measurement(deca_su_t100, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 100, 0.71).
narrative_ontology:measurement(deca_su_t120, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 120, 0.63).
narrative_ontology:measurement(deca_su_t140, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 140, 0.55).
narrative_ontology:measurement(deca_su_t160, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 160, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decalogue_image_prohibition__moderate_iconoclast_reading, identity_coordination).
narrative_ontology:affects_constraint(decalogue_image_prohibition__moderate_iconoclast_reading, decalogue_image_prohibition__iconoclast_reading).
narrative_ontology:affects_constraint(decalogue_image_prohibition__moderate_iconoclast_reading, decalogue_image_prohibition__iconodule_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the image prohibition' covers three structurally distinct constraints differing on one predicate: what makes a representation prohibited (any material representation used in worship / the worship-act performed toward it / the medium's dimensionality). Each reading is a separate file with its own epsilon, beneficiaries, and victims; this file instantiates the moderate reading (dimensionality grades risk: statuary banned, flat imagery licensed). Siblings are linked so contamination analysis can trace how a crack in one reading's enforcement propagates to the others. Epsilon is never averaged across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
