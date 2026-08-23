% ============================================================================
% CONSTRAINT STORY: magna_carta_constraint_authority__parliamentary_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_constraint_authority__parliamentary_sovereignty_reading, []).

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
 *   constraint_id: magna_carta_constraint_authority__parliamentary_sovereignty_reading
 *   human_readable: Magna Carta Restraints Carried by Parliamentary Statute (Parliamentary Sovereignty Reading)
 *   domain: constitutional_history/legal_philosophy/political_theory
 *
 * SUMMARY:
 *   Under the parliamentary sovereignty reading, Magna Carta's restraints
 *   survive only as content absorbed into parliamentary statute: the
 *   due-process, lawful-taxation, and detention commitments of 1215 and its
 *   reissues are carried forward through the Confirmatio Cartarum, the Habeas
 *   Corpus Acts, the Bill of Rights settlement, and their statutory
 *   descendants. Parliament inherits the charter's constraint-authority
 *   wholesale and may revise or repeal any provision by ordinary majority;
 *   nothing binds a sitting Parliament against its successors, and no
 *   external body can overturn its enactments. The arrangement has a genuine
 *   coordination half — a single authoritative restraint corpus binding the
 *   executive without a codified constitution — and an extraction half: the
 *   same channel lets each governing majority define the scope of everyone's
 *   protection, so minorities hold only what current majorities choose to
 *   maintain. The claim and the metrics are authored independently: the
 *   tangled-rope claim states what I believe structurally true of this
 *   reading's arrangement, and the metric values state what I believe
 *   descriptively true of its operation across 1689-2026. This file is one
 *   reading of the magna_carta_constraint_authority kernel; the sibling
 *   readings are separate constraints in separate files.
 *
 * KEY AGENTS:
 *   - - parliament_as_institution: Agenda setter (institutional/arbitrage) — inherits and exclusively administers the absorbed restraint corpus
 *   - - governing_majority_coalitions: Primary beneficiary (powerful/arbitrage) — defines restraint scope per session
 *   - - british_monarchy: Converted beneficiary-bearer (institutional/identity_locked) — traded prerogative for statutory continuity
 *   - - uk_judiciary: Enforcing beneficiary (institutional/constrained) — administers the corpus without override power
 *   - - unprotected_minorities: Primary target (powerless/trapped) — holds only repealable protections
 *   - - colonial_subjects_without_representation: Historical target (powerless/constrained) — governed by unrepresentative statute
 *   - - devolved_legislatures: Delegated beneficiary (organized/trapped) — powers revocable by the granting body
 *   - - unrepresented_subjects: Excluded seat (powerless/trapped) — subject to outputs, absent from proceedings
 *   - - constitutional_scholars: Analytical observer — sees the full migration of authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.61).
domain_priors:suppression_score(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.55).
domain_priors:theater_ratio(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_constraint_authority__parliamentary_sovereignty_reading, "Magna Carta Restraints Carried by Parliamentary Statute (Parliamentary Sovereignty Reading)").
narrative_ontology:topic_domain(magna_carta_constraint_authority__parliamentary_sovereignty_reading, "constitutional_history/legal_philosophy/political_theory").

domain_priors:requires_active_enforcement(magna_carta_constraint_authority__parliamentary_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 'ca0b91da-86e1-4301-bc40-13142661568d').
narrative_ontology:cs_kernel_codification('ca0b91da-86e1-4301-bc40-13142661568d', fixed_text).
narrative_ontology:cs_authority_grounding('ca0b91da-86e1-4301-bc40-13142661568d', lineage).
narrative_ontology:cs_interpretation_layer_present('ca0b91da-86e1-4301-bc40-13142661568d').
narrative_ontology:cs_reading_relation('ca0b91da-86e1-4301-bc40-13142661568d', magna_carta_constraint_authority__living_constitutionalism_reading, forecloses).
narrative_ontology:cs_reading_relation('ca0b91da-86e1-4301-bc40-13142661568d', magna_carta_constraint_authority__feudal_obsolescence_reading, coexists_with).
narrative_ontology:cs_axiom('ca0b91da-86e1-4301-bc40-13142661568d', foundational, constraint_authority_flows_only_through_statute).
narrative_ontology:cs_axiom_status(constraint_authority_flows_only_through_statute, holdable).
narrative_ontology:cs_axiom_grounding('ca0b91da-86e1-4301-bc40-13142661568d', constraint_authority_flows_only_through_statute, conventional).
narrative_ontology:cs_axiom('ca0b91da-86e1-4301-bc40-13142661568d', foundational, no_parliament_may_bind_its_successors).
narrative_ontology:cs_axiom_status(no_parliament_may_bind_its_successors, holdable).
narrative_ontology:cs_axiom_grounding('ca0b91da-86e1-4301-bc40-13142661568d', no_parliament_may_bind_its_successors, conventional).
narrative_ontology:cs_axiom('ca0b91da-86e1-4301-bc40-13142661568d', secondary, crown_prerogative_exercises_only_within_statutory_grants).
narrative_ontology:cs_axiom_status(crown_prerogative_exercises_only_within_statutory_grants, holdable).
narrative_ontology:cs_axiom_grounding('ca0b91da-86e1-4301-bc40-13142661568d', crown_prerogative_exercises_only_within_statutory_grants, conventional).
narrative_ontology:cs_reference_frame('ca0b91da-86e1-4301-bc40-13142661568d', charter_fully_absorbed_into_supreme_parliament).
narrative_ontology:cs_drift_state('ca0b91da-86e1-4301-bc40-13142661568d', contemporary_post_brexit_restoration, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('ca0b91da-86e1-4301-bc40-13142661568d', '').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__parliamentary_sovereignty_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliament_as_institution).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__parliamentary_sovereignty_reading, governing_majority_coalitions).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__parliamentary_sovereignty_reading, british_monarchy).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__parliamentary_sovereignty_reading, uk_judiciary).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__parliamentary_sovereignty_reading, devolved_legislatures).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, unprotected_minorities).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, colonial_subjects_without_representation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, british_monarchy).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, devolved_legislatures).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliamentary_supremacy_doctrine).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__parliamentary_sovereignty_reading, legal_positivism_no_higher_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bicameral legislature acting as the single authoritative law-maker. It received the charter's restraint-content through centuries of statutory confirmation and amendment, and holds the power to revise, re-enact, or repeal any of it by ordinary majority. Nothing it enacts binds its successors, and no external body can overturn its enactments. Its members' careers, the institution's standing, and the entire uncodified constitution rest on retaining this exclusive authority.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliament_as_institution, agenda_setter,
    institutional, generational, arbitrage, national).

% Whichever party or coalition holds a Commons majority at a given time defines, through ordinary legislation, the current scope of executive restraint and citizen protection. While in office it can extend or withdraw protections at will; when it loses office it becomes subject to its successor's definitions. Its planning horizon runs election to election.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, governing_majority_coalitions, beneficiary,
    powerful, biographical, arbitrage, national).

% The crown surrendered absolute prerogative across the seventeenth century in exchange for statutory recognition, hereditary continuity, and financial security. It now exercises public functions only within statutory grants and must assent to whatever legislation reaches it. Leaving the arrangement would mean abolishing the monarchy itself; the institution and its constitutional role have fused.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, british_monarchy, beneficiary,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_constraint_authority__parliamentary_sovereignty_reading, british_monarchy, payer).

% The courts administer the statutory corpus that carries the old charter content — habeas corpus, due-process clauses, detention limits — and enforce executive compliance with it. They hold no power to strike down or override primary legislation; when statute speaks plainly they apply it even where it removes protections, as in the wartime detention cases. Their institutional domain and professional purpose depend on the statutory material they police.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, uk_judiciary, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_constraint_authority__parliamentary_sovereignty_reading, uk_judiciary, agenda_setter).

% Groups without dependable parliamentary champions — historically religious dissenters and Catholics, wartime detainees and enemy aliens, more recently asylum seekers, prisoners, and other low-leverage populations. Whatever protection they hold exists because a current majority enacted it, and it can be withdrawn the same way. Emigration is costly and partial; waiting for a friendlier majority is the main strategy available.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, unprotected_minorities, payer,
    powerless, biographical, trapped, national).

% Populations across the empire governed directly by Westminster statutes in which they held no vote and returned no members — American colonies before independence, Ireland before and after Union, India and the African and Caribbean territories. Restraint and exaction alike arrived as legislation drafted thousands of miles away; their exits ran through migration, revolt, or eventual decolonization, all slow and costly.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, colonial_subjects_without_representation, payer,
    powerless, generational, constrained, continental).

% The Scottish Parliament and the Welsh and Northern Irish assemblies exercise law-making powers granted by Westminster statutes and revocable by the same route. They deliver locally fitted policy but hold no guarantee against Westminster override, as the repeated suspension of Northern Ireland's assembly and the post-Brexit internal-market disputes showed.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, devolved_legislatures, beneficiary,
    organized, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(magna_carta_constraint_authority__parliamentary_sovereignty_reading, devolved_legislatures, payer).

% People subject to the arrangement's outputs who hold no vote and no parliamentary champion: children, many migrants and short-term residents, prisoners, and historically the propertyless and women before enfranchisement. They would object that consent mediated through Parliament never reached them, but they appear in the proceedings only as subjects of legislation, never as participants.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, unrepresented_subjects, excluded,
    powerless, biographical, trapped, national).

% Historians, legal theorists, and political scientists who trace how the charter's authority migrated into statute, measure what the arrangement protects and exposes, and publish the comparisons — Dicey's codification, the Jackson debates, the devolution litigation analyses — that the participating institutions cite or ignore.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliament_as_institution).
narrative_ontology:fixing_cost_class(magna_carta_constraint_authority__parliamentary_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single authoritative channel through which executive restraint is defined and renewed: due-process, lawful-taxation, and detention provisions originating in the charter are carried as statute, giving crown, courts, and subjects one predictable source of lawful constraint without requiring a codified constitution.
% TRANSFER_FUNCTION: Moves constraint-authority itself — the power to define lawful restraint — from hereditary and customary sources (charter, common-law custom, coronation oath) to whichever coalition commands a Commons majority; correspondingly moves protection from subjects-as-charter-rightsholders to subjects-as-majority-grants, and places the costs of restraint on whoever lacks votes or parliamentary champions.
% ABSENT_VOICES: The voteless and the unchampioned: pre-Reform-Act working classes, women before 1918, colonial peoples ruled by Westminster statute without representation, wartime detainees, and today's prisoners, migrants, and children — all subject to the arrangement's outputs yet absent from the room where restraint-scope is set. Their objection, that consent mediated through Parliament never reached them, is recorded nowhere in the arrangement's own proceedings.
% DISAPPEARANCE_RATIONALE: If the absorbed-restraint regime vanished overnight, the executive would face no statutory restraint corpus — habeas corpus, taxation consent, and detention limits would lose their carrier — courts would lose the body of law they administer, the monarchy's constitutional position would become untenable or absolutist, and devolved institutions would lose their legal basis. The entire uncodified constitution would rearrange around whatever raw prerogative and bare majoritarianism remained.
% FOUNDING_PROBLEM: King John's arbitrary exactions, foreign-war demands, and imprisonments without lawful process: binding a hereditary ruler who stood above all law, achieved in 1215 by baronial force through a written compact.
% FOUNDING_PROBLEM_CORROBORATION: Medieval historians documenting the 1215 grievances and the charter's repeated reissue attest the founding problem; constitutional-law scholarship and judicial findings (the prorogation judgment, the wartime-detention critiques) attest the recurring generic form. Corroboration comes from outside the benefiting parties — academic historiography and the courts' own adversarial record — not from Parliament's self-account.
narrative_ontology:disappearance_verdict(magna_carta_constraint_authority__parliamentary_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_constraint_authority__parliamentary_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.61, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_constraint_authority__parliamentary_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_constraint_authority__parliamentary_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(magna_carta_constraint_authority__parliamentary_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.61 is moderate-high: the arrangement delivers real executive restraint (habeas corpus, consent to taxation, detention limits carried in statute) while making every protection conditional on current-majority maintenance, so costs concentrate on those without electoral leverage. The series peaks in 1942, when wartime detention regulations were ratified by courts applying statute as spoken, and recovers only partially under the 1998 overlays. Suppression 0.55 records doctrinal exclusion of alternatives — entrenchment, judicial override of primary legislation, higher-law constitutionalism — plus periodic active enforcement of supremacy: the Irish coercion acts, wartime Regulation 18B, and the post-Brexit reassertion against devolution and judicial pushback. Theater 0.48 reflects the heavy civic-liturgy layer — anniversaries, heritage display, rhetorical invocation of the 1215 parchment — atop an operative content that lives in ordinary statute. Accessibility collapse 0.60: once supremacy is understood, higher-law alternatives are visible but doctrinally blocked, collapsed short of natural-law totality. Resistance 0.50: sustained opposition across the interval — Chartism, Irish nationalism, devolution movements, human-rights advocacy, and the Jackson-dicta jurisprudence questioning supremacy itself. Minority coalition potential stays low because the exposed populations are dispersed and cross-cutting (dissenters, detainees, migrants, and prisoners rarely share identity or timing), so the powerless seats have not aggregated. All three metric series share one ten-point grid (1689-2026) so temporal comparison reads aligned rows; the rise-fall-rise shape tracks crisis cycles and rival-authority episodes (the world wars, the EU/HRA overlay, Brexit restoration) rather than oscillating reinforcement.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from identical structure. From a governing-majority seat the arrangement is self-government: the coalition's will defines restraint, so the same repealability that threatens minorities reads as democratic responsiveness. From the unprotected-minority seat the identical structure is conditional protection, revocable at opponents' pleasure — restraint experienced as weather, not shelter. The monarchy experiences the arrangement as a completed trade (prerogative for continuity) and reports it as legitimate settlement; the judiciary experiences administration without adjudication — enforcing limits it did not set and cannot deepen. The engine derives these divergences from the role, power, and exit data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations drive the derivation. Parliament sits nearest the beneficiary pole: it collects the transferred authority itself and faces no revisionary check. Governing coalitions inherit that position each session with arbitrage-grade control. The monarchy declares beneficiary with a payer secondary — it pays in prerogative but collects continuity and security — placing it mid-scale rather than at either pole. The judiciary takes a mild beneficiary position, its institutional domain funded by the corpus it administers. Unprotected minorities and colonial subjects sit nearest the target pole; trapped and constrained exits respectively push their effective extraction upward. Devolved legislatures take a mild beneficiary position with payer exposure. Unrepresented subjects are authored as an excluded seat — commentary-grade absence, not a correction-grade input. No directionality_overrides are authored: the three institutional-atom seats (parliament, monarchy, judiciary) differentiate through role and exit declarations, and an override keyed to the shared institutional power atom would misapply across all three.
 *
 * MANDATROPHY ANALYSIS:
 *   Classification discipline: calling the arrangement a rope (a nation solving restraint-on-executive collectively) would erase the repealability channel through which majorities withdraw protection from minorities; calling it a snare would erase the genuine, centuries-delivered restraint that no rival arrangement provided. Tangled rope holds both halves: a coordination function (single authoritative restraint corpus) and asymmetric extraction (authority and protection flow to those who command the channel). On mandatrophy: the founding problem in its feudal form — a lawless hereditary king — is dead; its generic form — the unconstrained executive — recurs (the 2019 prorogation, wartime powers, counter-terrorism detention), so founding_problem_status is contested rather than dead, and the mismatch consumer finds contested paired with world_rearranges, asserting no zombie flag. The theater signal is layered: the symbolic charter (parchment, anniversaries, civic liturgy) drifts toward performance while the statutory layer still functions — a piton-flavored symptom confined to the symbol, tracked by the symbolic_functional_split omega rather than read as whole-constraint decay.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_structural_delta,
    'How would the victim set and epsilon shift under the sibling readings of the magna_carta_constraint_authority kernel — judicially evolved restraint (living constitutionalism) or no surviving restraint (feudal obsolescence)?',
    'Compile the sibling stories and compare computed per-seat classifications over the same referent; the delta in victim sets and effective extraction across readings is the kernel contest made measurable.',
    'If the living-constitutionalism reading computes lower extraction for minorities, the sovereignty reading''s repealability channel is confirmed as the extraction site; if the obsolescence reading computes comparable extraction from ordinary statute alone, the charter label contributes nothing structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Committer-frame omega: this story is one reading of the kernel; sibling readings restructure the victim set and epsilon.').

omega_variable(
    repealability_specific_harm_share,
    'What share of harm to unprotected minorities traces specifically to the no-entrenchment/repealability structure, rather than to legislative politics that any constitutional form would permit?',
    'Comparative constitutional analysis: matched minority outcomes under entrenched constitutions versus the UK arrangement across equivalent periods.',
    'A high repealability-specific share raises epsilon toward the extractive end and strengthens the case for entrenchment remedies; a low share lowers epsilon toward coordination-cost territory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(repealability_specific_harm_share, empirical, 'Whether the arrangement''s signature structure causes distinguishable marginal harm.').

omega_variable(
    absorption_completeness,
    'Is absorption complete — does any restraint now bind Parliament itself (common-law constitutional rights, international obligations, the Jackson-dicta possibility) — or does the whole residue remain at Parliament''s disposal?',
    'Track whether any court ever refuses effect to express Westminster statutory language on constitutional-rights grounds, and whether any entrenchment attempt survives a successor majority.',
    'Complete absorption confirms the tangled-rope structure with Parliament as sole inheritor; demonstrated judicial retention would migrate the story toward the living-constitutionalism reading''s structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absorption_completeness, empirical, 'Whether any fragment of the charter''s authority escaped the statutory carrier.').

omega_variable(
    consent_mediation_filter,
    'Does mediation of popular will through Parliament aggregate dispersed preferences into restraint, or filter out the interests of those without votes and champions?',
    'Preference- and design-dependent: compare policy responsiveness to low-leverage populations under parliamentary mediation versus direct-democratic or judicial-protection channels.',
    'If mediation aggregates, the coordination credit stands and epsilon rests on the residual; if it filters, part of the measured extraction is the mediation structure itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consent_mediation_filter, preference, 'Whether the mediation bottleneck is aggregation or filtration.').

omega_variable(
    symbolic_functional_split,
    'Is the elevated theater ratio decay of the charter''s distinct normative identity (performance replacing function at the symbolic layer) or healthy civic ritual that sustains compliance with the statutory corpus?',
    'Test whether commemorative and rhetorical invocation correlates with compliance and protection outcomes or substitutes for them; track whether distinctively charter-originated provisions retain identifiable content after successive absorptions.',
    'Ritual-that-sustains supports the tangled-rope reading with the symbol as maintenance overhead; performance-replacing-identity vindicates the feudal-obsolescence sibling and marks the symbol as inertial residue.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbolic_functional_split, conceptual, 'Whether the theatrical layer is maintenance overhead or atrophied residue.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 1689, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mc_parl_sovereignty_tr_t1689, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 1689, 0.2).
narrative_ontology:measurement_basis(mc_parl_sovereignty_tr_t1689, observed).
narrative_ontology:measurement(mc_parl_sovereignty_tr_t1745, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 1745, 0.24).
narrative_ontology:measurement_basis(mc_parl_sovereignty_tr_t1745, observed).
narrative_ontology:measurement(mc_parl_sovereignty_tr_t1800, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 1800, 0.3).
narrative_ontology:measurement_basis(mc_parl_sovereignty_tr_t1800, observed).
narrative_ontology:measurement(mc_parl_sovereignty_tr_t1840, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 1840, 0.34).
narrative_ontology:measurement_basis(mc_parl_sovereignty_tr_t1840, observed).
narrative_ontology:measurement(mc_parl_sovereignty_tr_t1885, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 1885, 0.38).
narrative_ontology:measurement_basis(mc_parl_sovereignty_tr_t1885, observed).
narrative_ontology:measurement(mc_parl_sovereignty_tr_t1918, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 1918, 0.4).
narrative_ontology:measurement_basis(mc_parl_sovereignty_tr_t1918, observed).
narrative_ontology:measurement(mc_parl_sovereignty_tr_t1942, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 1942, 0.38).
narrative_ontology:measurement_basis(mc_parl_sovereignty_tr_t1942, observed).
narrative_ontology:measurement(mc_parl_sovereignty_tr_t1998, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 1998, 0.44).
narrative_ontology:measurement_basis(mc_parl_sovereignty_tr_t1998, observed).
narrative_ontology:measurement(mc_parl_sovereignty_tr_t2020, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 2020, 0.47).
narrative_ontology:measurement_basis(mc_parl_sovereignty_tr_t2020, observed).
narrative_ontology:measurement(mc_parl_sovereignty_tr_t2026, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 2026, 0.48).
narrative_ontology:measurement_basis(mc_parl_sovereignty_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(mc_parl_sovereignty_be_t1689, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 1689, 0.42).
narrative_ontology:measurement_basis(mc_parl_sovereignty_be_t1689, observed).
narrative_ontology:measurement(mc_parl_sovereignty_be_t1745, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 1745, 0.46).
narrative_ontology:measurement_basis(mc_parl_sovereignty_be_t1745, observed).
narrative_ontology:measurement(mc_parl_sovereignty_be_t1800, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 1800, 0.52).
narrative_ontology:measurement_basis(mc_parl_sovereignty_be_t1800, observed).
narrative_ontology:measurement(mc_parl_sovereignty_be_t1840, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 1840, 0.55).
narrative_ontology:measurement_basis(mc_parl_sovereignty_be_t1840, observed).
narrative_ontology:measurement(mc_parl_sovereignty_be_t1885, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 1885, 0.57).
narrative_ontology:measurement_basis(mc_parl_sovereignty_be_t1885, observed).
narrative_ontology:measurement(mc_parl_sovereignty_be_t1918, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 1918, 0.6).
narrative_ontology:measurement_basis(mc_parl_sovereignty_be_t1918, observed).
narrative_ontology:measurement(mc_parl_sovereignty_be_t1942, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 1942, 0.66).
narrative_ontology:measurement_basis(mc_parl_sovereignty_be_t1942, observed).
narrative_ontology:measurement(mc_parl_sovereignty_be_t1998, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 1998, 0.56).
narrative_ontology:measurement_basis(mc_parl_sovereignty_be_t1998, observed).
narrative_ontology:measurement(mc_parl_sovereignty_be_t2020, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 2020, 0.58).
narrative_ontology:measurement_basis(mc_parl_sovereignty_be_t2020, observed).
narrative_ontology:measurement(mc_parl_sovereignty_be_t2026, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 2026, 0.61).
narrative_ontology:measurement_basis(mc_parl_sovereignty_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(mc_parl_sovereignty_su_t1689, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1689, 0.35).
narrative_ontology:measurement_basis(mc_parl_sovereignty_su_t1689, observed).
narrative_ontology:measurement(mc_parl_sovereignty_su_t1745, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1745, 0.4).
narrative_ontology:measurement_basis(mc_parl_sovereignty_su_t1745, observed).
narrative_ontology:measurement(mc_parl_sovereignty_su_t1800, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1800, 0.48).
narrative_ontology:measurement_basis(mc_parl_sovereignty_su_t1800, observed).
narrative_ontology:measurement(mc_parl_sovereignty_su_t1840, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1840, 0.44).
narrative_ontology:measurement_basis(mc_parl_sovereignty_su_t1840, observed).
narrative_ontology:measurement(mc_parl_sovereignty_su_t1885, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1885, 0.5).
narrative_ontology:measurement_basis(mc_parl_sovereignty_su_t1885, observed).
narrative_ontology:measurement(mc_parl_sovereignty_su_t1918, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1918, 0.62).
narrative_ontology:measurement_basis(mc_parl_sovereignty_su_t1918, observed).
narrative_ontology:measurement(mc_parl_sovereignty_su_t1942, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1942, 0.68).
narrative_ontology:measurement_basis(mc_parl_sovereignty_su_t1942, observed).
narrative_ontology:measurement(mc_parl_sovereignty_su_t1998, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1998, 0.38).
narrative_ontology:measurement_basis(mc_parl_sovereignty_su_t1998, observed).
narrative_ontology:measurement(mc_parl_sovereignty_su_t2020, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 2020, 0.52).
narrative_ontology:measurement_basis(mc_parl_sovereignty_su_t2020, observed).
narrative_ontology:measurement(mc_parl_sovereignty_su_t2026, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 2026, 0.55).
narrative_ontology:measurement_basis(mc_parl_sovereignty_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_constraint_authority__parliamentary_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__parliamentary_sovereignty_reading, living_constitutionalism_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__parliamentary_sovereignty_reading, feudal_obsolescence_reading).

% DUAL FORMULATION NOTE:
% Constraint family: one kernel (magna_carta_constraint_authority), three readings emitted as separate files. This file (parliamentary_sovereignty_reading) links both siblings. Upstream/downstream structure: the feudal-obsolescence reading is the skeptical baseline (nothing survives); the parliamentary-sovereignty reading absorbs (everything survives as statute); the living-constitutionalism reading resists absorption (something binds outside statute). The sovereignty reading's completion of absorption is precisely what feeds the obsolescence claim — full absorption leaves nothing distinctively charter-like — while judicial-retention evidence feeds the living-constitutionalism claim. Epsilons differ by reading over the same referent; no file averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
