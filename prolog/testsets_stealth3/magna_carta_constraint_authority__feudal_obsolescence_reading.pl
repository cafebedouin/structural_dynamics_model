% ============================================================================
% CONSTRAINT STORY: magna_carta_constraint_authority__feudal_obsolescence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_constraint_authority__feudal_obsolescence_reading, []).

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
 *   constraint_id: magna_carta_constraint_authority__feudal_obsolescence_reading
 *   human_readable: Magna Carta Demotion Arrangement (Feudal Obsolescence Reading)
 *   domain: constitutional_history/legal_philosophy/political_theory
 *
 * SUMMARY:
 *   The standing arrangement under contest is the modern disposition of Magna
 *   Carta: a text retained in fragmentary statutory form, celebrated as the
 *   fountainhead of lawful restraint, and enforced against no modern
 *   institution. This file instantiates the feudal_obsolescence_reading of
 *   the magna_carta_constraint_authority kernel, which holds the charter to
 *   be a baronial-fiscal compact whose binding force expired with the feudal
 *   order that produced it. The sibling readings — living constitutionalism
 *   and parliamentary sovereignty — are separate constraint files linked
 *   through network.affects_constraints; no sibling's evaluation is averaged
 *   into this one, and this file's epsilon is authored for this reading alone
 *   over the shared referent (the standing arrangement). Assessed by this
 *   reading's own lights, the arrangement's defining feature is the
 *   disproportion between maintenance and function: the commemorative
 *   apparatus is vast, the operative footprint is nil, and the gap between
 *   promised and delivered restraint falls on those without a forum. KEY
 *   AGENTS (by structural relationship): - modern_executive_governments:
 *   Primary beneficiary (institutional/arbitrage) — harvests the charter's
 *   legitimating symbolism while operating unbound by it - uk_parliament:
 *   Agenda setter and secondary beneficiary (institutional/mobile) —
 *   administers the arrangement; retains or repeals charter statutes at will
 *   - uk_judiciary: Doctrinal gatekeeper (institutional/constrained) —
 *   declines to enforce charter clauses as freestanding law -
 *   popular_constitutionalism_advocates: Payer (organized/identity_locked) —
 *   ground rights claims in a legally inert instrument -
 *   charter_based_litigants: Payer (powerless/trapped) — invoke clauses 39/40
 *   against state action; citations treated as ornamental -
 *   common_law_constitutionalists: Payer (moderate/identity_locked) — jurists
 *   whose fundamental-law tradition is filed under history -
 *   ordinary_subjects_citizens: Payer and excluded voice (powerless/trapped)
 *   — bear the diffuse gap between promised and delivered restraint -
 *   heritage_commemoration_establishment: Beneficiary (moderate/mobile) —
 *   sustains institutions on the charter's ceremonial life -
 *   constitutional_historians: Analytical observer — document the gap between
 *   operative force and ceremonial maintenance
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.6).
domain_priors:suppression_score(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.48).
domain_priors:theater_ratio(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 0.78).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__feudal_obsolescence_reading, piton).
narrative_ontology:human_readable(magna_carta_constraint_authority__feudal_obsolescence_reading, "Magna Carta Demotion Arrangement (Feudal Obsolescence Reading)").
narrative_ontology:topic_domain(magna_carta_constraint_authority__feudal_obsolescence_reading, "constitutional_history/legal_philosophy/political_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__feudal_obsolescence_reading, '0656d3db-53bf-4894-a7c6-d3a61c71d036').
narrative_ontology:cs_kernel_codification('0656d3db-53bf-4894-a7c6-d3a61c71d036', fixed_text).
narrative_ontology:cs_authority_grounding('0656d3db-53bf-4894-a7c6-d3a61c71d036', expertise).
narrative_ontology:cs_interpretation_layer_present('0656d3db-53bf-4894-a7c6-d3a61c71d036').
narrative_ontology:cs_reading_relation('0656d3db-53bf-4894-a7c6-d3a61c71d036', magna_carta_constraint_authority__living_constitutionalism_reading, forecloses).
narrative_ontology:cs_reading_relation('0656d3db-53bf-4894-a7c6-d3a61c71d036', magna_carta_constraint_authority__parliamentary_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('0656d3db-53bf-4894-a7c6-d3a61c71d036', foundational, authority_exhausted_with_feudal_context).
narrative_ontology:cs_axiom_status(authority_exhausted_with_feudal_context, holdable).
narrative_ontology:cs_axiom_grounding('0656d3db-53bf-4894-a7c6-d3a61c71d036', authority_exhausted_with_feudal_context, empirically_contingent).
narrative_ontology:cs_axiom('0656d3db-53bf-4894-a7c6-d3a61c71d036', foundational, sovereignty_derives_from_present_authorization_not_charter_grant).
narrative_ontology:cs_axiom_status(sovereignty_derives_from_present_authorization_not_charter_grant, holdable).
narrative_ontology:cs_axiom_grounding('0656d3db-53bf-4894-a7c6-d3a61c71d036', sovereignty_derives_from_present_authorization_not_charter_grant, conventional).
narrative_ontology:cs_reference_frame('0656d3db-53bf-4894-a7c6-d3a61c71d036', context_bound_feudal_compact).
narrative_ontology:cs_drift_state('0656d3db-53bf-4894-a7c6-d3a61c71d036', contemporary_doctrine, gap(repudiation_pressure, minor, true)).
narrative_ontology:cs_created_at('0656d3db-53bf-4894-a7c6-d3a61c71d036', '').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__feudal_obsolescence_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__feudal_obsolescence_reading, modern_executive_governments).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__feudal_obsolescence_reading, uk_parliament).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__feudal_obsolescence_reading, heritage_commemoration_establishment).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, popular_constitutionalism_advocates).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, charter_based_litigants).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, common_law_constitutionalists).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, ordinary_subjects_citizens).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__feudal_obsolescence_reading, historical_contextualist_jurisprudence).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__feudal_obsolescence_reading, positive_law_sovereignty_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Governs under a doctrine of unbounded parliamentary-executive competence. Invokes the charter at ceremonies, anniversaries, and in rhetoric about lawful government, while relying on the fact that no court will enforce its clauses against ministerial action. When litigants or campaigners press charter text against policy, government counsel answers that the document is historical. The arrangement costs this seat nothing and yields symbolic capital on demand.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, modern_executive_governments, beneficiary,
    institutional, biographical, arbitrage, national).

% Holds the statutory relics of the charter on the books and could extend, entrench, or repeal any of them by ordinary procedure; chooses, session after session, to do none of these. Carried out the repeals that removed most clauses as obsolete while preserving the ceremonial core. Operates in a constitutional order in which no superior fundamental law competes with statute.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, uk_parliament, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_constraint_authority__feudal_obsolescence_reading, uk_parliament, beneficiary).

% Decides in practice what the charter's force is: treats surviving clauses as historical context rather than freestanding law, folding any live principle into common-law or statutory channels where parliamentary intent controls. Cannot unilaterally restore independent charter force against the sovereignty doctrine and does not attempt to; retains interpretive custody of the question by keeping it open in form and closed in effect.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, uk_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Campaign groups, lawyers, and writers who ground claims about rights and lawful restraint in the charter's continuing promise. Their instrument is legally inert: petitions and campaigns citing it are received as sentiment, not argument. Leaving the frame would mean abandoning the founding text of their constitutional tradition, so they continue to invoke a document the institutions they address treat as ornament.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, popular_constitutionalism_advocates, payer,
    organized, generational, identity_locked, national).

% Individuals facing detention, seizure, or administrative sanction who cite clauses 39 and 40 in their own defense. Their citations appear in judgments as historical color while the decision turns on statute and common law. They arrive at the arrangement already caught in state power — that is why they litigate — and leave it with the same exposure they brought.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, charter_based_litigants, payer,
    powerless, immediate, trapped, national).

% Jurists and scholars working in the tradition that holds some fundamental law stands above ordinary statute. Their canon runs through the charter, and its dismissal as a feudal curiosity strips the tradition of its foundational exhibit. Retiring the tradition would dissolve careers built inside it; continuing inside it means arguing from a text the profession files under history.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, common_law_constitutionalists, payer,
    moderate, biographical, identity_locked, national).

% Live under guarantees of lawful judgment celebrated in school curricula and state ceremony and enforced by no mechanism they can reach. Bear the difference between the restraint promised and the restraint delivered as a diffuse background condition: nothing they can point to is taken from them, and nothing they can invoke checks the state when it acts. They have no forum in which their objection to the gap counts as a constitutional claim.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, ordinary_subjects_citizens, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_constraint_authority__feudal_obsolescence_reading, ordinary_subjects_citizens, excluded).

% Trusts, museums, visitor sites, and anniversary committees whose programming, funding, and attendance rest on the charter's symbolic life. Every jubilee and exhibition converts the document's fame into revenue and relevance. Their interest lies in the charter remaining famous rather than feared; enforcement would change their relationship to it not at all.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, heritage_commemoration_establishment, beneficiary,
    moderate, biographical, mobile, regional).

% Study the charter's drafting, reissues, enforcement record, and afterlife from outside the dispute over its present force. Document the gap between what the text did when its enforcement machinery existed and what it does now, and publish the finding that its operative career ended centuries before its commemorative career began.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_constraint_authority__feudal_obsolescence_reading, modern_executive_governments).
narrative_ontology:fixing_cost_class(magna_carta_constraint_authority__feudal_obsolescence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates elite constitutional continuity: supplies all institutions a shared origin narrative that each can invoke without any being bound, lowering the cost of constitutional argument by giving every faction a common reference point that obligates none of them.
% TRANSFER_FUNCTION: Moves discretionary latitude from the governed to executive and legislative institutions, who operate without a superior-law check; moves symbolic legitimacy from a medieval bargaining document to modern political elites, who cite it while owing it nothing; moves attention and funds to the commemoration sector; moves nothing material to any charter-invoking claimant.
% ABSENT_VOICES: Those with the strongest reason to object stand outside the rooms where the arrangement is administered: charter-invoking litigants learn from judgment transcripts that their citations carried no weight; the charter's own historic outsiders — the unfree, to whom clause 39's 'free man' never extended — have no descendant seat at any commemorative or doctrinal table; and holders of rival accounts of the charter's authority shape commentary but hold no operative lever, since access to force runs through Parliament and the courts. Unanimity about the arrangement's harmlessness arises partly because the seats that would dispute it were never given a channel that counts.
% DISAPPEARANCE_RATIONALE: Repeal the relic clauses and cancel the ceremonies overnight and no court's decision changes, no statute's validity lapses, no executive power shrinks: the operative world runs on statute, common law, and administrative practice that never consulted the charter. What would need replacing is symbolic — speechwriters would migrate to the 1689 Bill of Rights and common-law mythology within a news cycle, and the commemoration sector would re-anchor on other anniversaries. The arrangement is load-bearing for narrative, not for governance.
% FOUNDING_PROBLEM: King John's fiscal and judicial predation on his barons: scutage levied without consent, punitive reliefs and wardships, arbitrary seizure and imprisonment without lawful judgment. The 1215 compact traded specified concessions and enforcement machinery for peace; the reissues of 1216, 1217, 1225, and 1297 narrowed it to the crown-baron bargain each reign could live with.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians, sitting outside every benefiting party, attest that the founding problem was feudal-fiscal and is extinct. Parliament's own repeal record is a second external attestation: the administrator itself struck out the charter's operative clauses as obsolete across the nineteenth-century statute revisions. No litigant, campaigner, or beneficiary claims that scutage, wardship, or baronial-relief grievances persist; the beneficiaries attest only symbolic continuity, which is a different claim about a different object.
narrative_ontology:disappearance_verdict(magna_carta_constraint_authority__feudal_obsolescence_reading, world_unchanged).
narrative_ontology:founding_problem_status(magna_carta_constraint_authority__feudal_obsolescence_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_constraint_authority__feudal_obsolescence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(magna_carta_constraint_authority__feudal_obsolescence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_constraint_authority__feudal_obsolescence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_constraint_authority__feudal_obsolescence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(magna_carta_constraint_authority__feudal_obsolescence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.60: the arrangement's cost to the governed is opportunity-shaped rather than flow-shaped — a celebrated check on state power exists only as decoration, and the political class harvests the text's prestige while owing it nothing. It is moderate-to-high rather than severe because no material good is transferred out of any victim's hands by the arrangement itself; the loss is the restraint that never arrives. Suppression is 0.48 and is doctrinal, not physical: the closure operates through interpretive authority (courts declining enforcement, the profession filing the text under history), and per the framework's rule it is authored as a raw structural property, unscaled by power or scope. Theater is 0.78 — the arrangement's contemporary activity is overwhelmingly commemorative: jubilees, exhibitions, anniversaries, rhetorical invocation, with the statutory relics untouched precisely because touching them would force the functionlessness into the open. Accessibility collapse is 0.55: once one understands the arrangement, the litigation route to charter restraint is closed (no modern judgment turns on the text), but discursive and political alternatives — statutory rights instruments, common-law constitutional argument — remain open, so alternatives are partly collapsed, not wholly. Resistance is 0.52: periodic invocation against detention, prorogation, and forfeiture, scholarly defense of the ancient constitution, recurring commemorative politics — real, recurrent, and consistently ineffective through the charter channel itself. Identity-lock dynamics bind two payer seats: for the advocates the lock is ideological (the charter is the founding frame of their constitutionalism; exit means abandoning the tradition's premise), and for the common-law constitutionalists it is professional (careers and canons built inside the fundamental-law lineage; exit means dissolving the tradition's foundational exhibit). If either lock broke, those seats would migrate their claims to statutory instruments and the arrangement would lose its remaining internal critics. The measurement series run on one shared nine-point grid (1297–2025) so that every tracked metric is authored at every examined time point; the trajectories tell one story: enforcement machinery decays monotonically (suppression_requirement falling from 0.68 to 0.06 as baronial enforcement, then judicial enforcement, then any enforcement disappears), theater rises monotonically (0.06 to 0.78) as commemoration replaces compulsion, and the arrangement's cost to the governed accumulates (0.18 to 0.60) as the check thins while the celebration thickens. The dynamics are monotone drift, not cyclical, so no oscillation analysis applies.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the administrator/beneficiary seats should compute differently, and the structural data explains why. From the parliament and executive seats the arrangement is settled constitutional housekeeping: a relic maintained at negligible cost, yielding a shared narrative that obligates no one — nothing about their situation registers as bearing a burden. From the charter_based_litigant seat the same arrangement is a broken promise administered as ornament: they arrive caught in state power, invoke the celebrated guarantee, and leave with their exposure unchanged. From the advocate and scholar seats it is the slow erasure of a tradition's foundational instrument. Same text, same doctrine, opposite experiences — the engine computes this divergence from power, exit, and role data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the low-directionality seats: the executive (arbitrage exit — it exploits the gap between the text's fame and its nullity, the nearest-to-full-beneficiary position in the story), parliament (agenda setter collecting both administration and benefit), and the heritage establishment (mobile, collecting the ceremonial economy). Victim declarations drive the high-directionality seats: litigants and ordinary citizens (trapped — they cannot exit the jurisdiction or the state's power, and their harm is the foregone restraint), advocates and common-law constitutionalists (identity_locked — their harm is the devaluation of the instrument their identity is built on, and identity lock sits them nearer the full-target end than their mobility alone would). The judiciary declares no beneficiary or victim position: it administers the demotion and collects interpretive custody of the question, placing it near symmetric. No directionality_overrides entries are authored: the derivation chain from beneficiary/victim declarations plus exit options produces the correct qualitative ordering for every seat, and an override keyed only to a power atom would misfire across the three institutional seats (parliament, executive, judiciary) that share the institutional atom but hold different structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding mandate — relieving baronial fiscal and judicial grievances — is extinct beyond reconstruction: no modern actor faces scutage, wardship, or baronial relief, and the administrator's own repeal record attests the obsolescence. What persists is the container: relic clauses nobody enforces or repeals, ceremonies nobody needs, a founding myth every faction cites and none obeys. Classifying this as a piton prevents two opposite mislabels. Reading the reverence at face value would mistake a cherished document for a functioning restraint (a rope that does not restrain); reading the executive's harvested discretion at face value would mistake diffuse opportunity-cost and ceremonial convenience for a coerced extraction machine (a snare whose persistence requires no coercion because nobody with power is hurt enough to fix it and no seat captures enough to defend it beyond routine). The receipt surface records the honest complication: the executive seat demonstrably receives the arrangement's principal gains (discretion maximized relative to any counterfactual where the charter binds), while the fix — restoring binding force — would require dismantling parliamentary sovereignty itself, a constitutional revolution, hence prohibitive. The R5 interview closes the loop coherently: founding problem dead, disappearance verdict world_unchanged — a dead mandate over an arrangement whose vanishing would rearrange narrative, not governance. That is the vestigial profile, and it is why the mandatrophy is recorded as resolved rather than pending.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading (feudal_obsolescence_reading) of the magna_carta_constraint_authority kernel; would instantiating a sibling reading instead — living constitutionalism or parliamentary sovereignty — change the victim/beneficiary structure and the classification outright?',
    'Not resolvable within this framework: the disagreement is located in whether authority requires present-day enforceability or can persist through interpretive lineage or parliamentary absorption. Resolution occurs only if one reading displaces the others in operative doctrine.',
    'Under the living-constitutionalism sibling the executive becomes the target seat and charter-invoking citizens become beneficiaries, inverting this story''s directionality map; under the parliamentary-sovereignty sibling the arrangement merges into statute and this story''s standalone victim set dissolves. Neither displacement leaves this file''s classification intact.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer-frame omega: this story is one of three readings of the Magna Carta authority kernel; sibling readings are separate constraints with different epsilon and different victim sets.').

omega_variable(
    inertia_vs_active_maintenance,
    'Does the arrangement persist by pure institutional inertia, or do its beneficiaries actively maintain the obsolescence doctrine in order to preserve the discretion and symbolic capital it yields?',
    'Track defensive activity over time: ministerial responses to charter-based legal claims, court guidance treating charter citations as historical, parliamentary handling of proposals to entrench or revive surviving clauses. Active defense beyond passive neglect indicates maintenance; indifference indicates inertia.',
    'Demonstrable active maintenance, combined with the executive seat''s captured gains, would shift the computed classification from the atrophied-restraint profile toward a coerced-extraction profile; confirmed inertia confirms the piton reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inertia_vs_active_maintenance, empirical, 'Whether persistence is inertial drift or defended extraction.').

omega_variable(
    residual_operative_force,
    'Do any surviving 1297 clauses retain non-trivial operative force — for example through the due process jurisprudence descended from clause 39 — or is the operative content of the text nil?',
    'Audit modern case law for judgments that turn on charter text itself rather than on its statutory or common-law descendants; distinguish causal lineage (clause 39 shaping a tradition) from operative reliance (a decision that would fail without the clause).',
    'Non-trivial operative reliance would lower the theater measure and pull the classification toward a hybrid coordination-plus-residue profile; nil reliance confirms the atrophied-restraint reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_operative_force, empirical, 'Whether any clause still does work or the text is wholly ceremonial.').

omega_variable(
    coverage_gap_inheritance,
    'Clause 39''s guarantees reached only the free — villeins, and in key respects women and Jews, stood outside the chartered class; should the arrangement''s costs be assessed against the whole governed population or only against the descendants of the formally included?',
    'No empirical test settles this: it is a normative choice about the relevant population for measuring the gap between promised and delivered restraint. Framing it explicitly is the resolution.',
    'Whole-population assessment raises the measured cost borne under the arrangement (many more people lived under the gap); included-class assessment lowers it. The classification''s extractiveness reading moves with the choice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coverage_gap_inheritance, preference, 'Whose foregone restraint counts: the chartered class or everyone governed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__feudal_obsolescence_reading, 1297, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mc_feudal_obsolescence_tr_t1297, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1297, 0.06).
narrative_ontology:measurement_basis(mc_feudal_obsolescence_tr_t1297, observed).
narrative_ontology:measurement(mc_feudal_obsolescence_tr_t1500, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1500, 0.14).
narrative_ontology:measurement_basis(mc_feudal_obsolescence_tr_t1500, observed).
narrative_ontology:measurement(mc_feudal_obsolescence_tr_t1642, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1642, 0.24).
narrative_ontology:measurement_basis(mc_feudal_obsolescence_tr_t1642, observed).
narrative_ontology:measurement(mc_feudal_obsolescence_tr_t1689, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1689, 0.33).
narrative_ontology:measurement_basis(mc_feudal_obsolescence_tr_t1689, observed).
narrative_ontology:measurement(mc_feudal_obsolescence_tr_t1800, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1800, 0.47).
narrative_ontology:measurement_basis(mc_feudal_obsolescence_tr_t1800, observed).
narrative_ontology:measurement(mc_feudal_obsolescence_tr_t1911, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1911, 0.62).
narrative_ontology:measurement_basis(mc_feudal_obsolescence_tr_t1911, observed).
narrative_ontology:measurement(mc_feudal_obsolescence_tr_t1965, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1965, 0.7).
narrative_ontology:measurement_basis(mc_feudal_obsolescence_tr_t1965, observed).
narrative_ontology:measurement(mc_feudal_obsolescence_tr_t2005, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 2005, 0.76).
narrative_ontology:measurement_basis(mc_feudal_obsolescence_tr_t2005, observed).
narrative_ontology:measurement(mc_feudal_obsolescence_tr_t2025, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 2025, 0.78).
narrative_ontology:measurement_basis(mc_feudal_obsolescence_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(mc_feudal_obsolescence_be_t1297, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1297, 0.18).
narrative_ontology:measurement_basis(mc_feudal_obsolescence_be_t1297, observed).
narrative_ontology:measurement(mc_feudal_obsolescence_be_t1500, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1500, 0.22).
narrative_ontology:measurement_basis(mc_feudal_obsolescence_be_t1500, observed).
narrative_ontology:measurement(mc_feudal_obsolescence_be_t1642, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1642, 0.3).
narrative_ontology:measurement_basis(mc_feudal_obsolescence_be_t1642, observed).
narrative_ontology:measurement(mc_feudal_obsolescence_be_t1689, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1689, 0.34).
narrative_ontology:measurement_basis(mc_feudal_obsolescence_be_t1689, observed).
narrative_ontology:measurement(mc_feudal_obsolescence_be_t1800, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1800, 0.42).
narrative_ontology:measurement_basis(mc_feudal_obsolescence_be_t1800, observed).
narrative_ontology:measurement(mc_feudal_obsolescence_be_t1911, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1911, 0.5).
narrative_ontology:measurement_basis(mc_feudal_obsolescence_be_t1911, observed).
narrative_ontology:measurement(mc_feudal_obsolescence_be_t1965, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1965, 0.56).
narrative_ontology:measurement_basis(mc_feudal_obsolescence_be_t1965, observed).
narrative_ontology:measurement(mc_feudal_obsolescence_be_t2005, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 2005, 0.59).
narrative_ontology:measurement_basis(mc_feudal_obsolescence_be_t2005, observed).
narrative_ontology:measurement(mc_feudal_obsolescence_be_t2025, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 2025, 0.6).
narrative_ontology:measurement_basis(mc_feudal_obsolescence_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(mc_feudal_obsolescence_su_t1297, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 1297, 0.68).
narrative_ontology:measurement_basis(mc_feudal_obsolescence_su_t1297, observed).
narrative_ontology:measurement(mc_feudal_obsolescence_su_t1500, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 1500, 0.52).
narrative_ontology:measurement_basis(mc_feudal_obsolescence_su_t1500, observed).
narrative_ontology:measurement(mc_feudal_obsolescence_su_t1642, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 1642, 0.44).
narrative_ontology:measurement_basis(mc_feudal_obsolescence_su_t1642, observed).
narrative_ontology:measurement(mc_feudal_obsolescence_su_t1689, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 1689, 0.32).
narrative_ontology:measurement_basis(mc_feudal_obsolescence_su_t1689, observed).
narrative_ontology:measurement(mc_feudal_obsolescence_su_t1800, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 1800, 0.22).
narrative_ontology:measurement_basis(mc_feudal_obsolescence_su_t1800, observed).
narrative_ontology:measurement(mc_feudal_obsolescence_su_t1911, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 1911, 0.14).
narrative_ontology:measurement_basis(mc_feudal_obsolescence_su_t1911, observed).
narrative_ontology:measurement(mc_feudal_obsolescence_su_t1965, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 1965, 0.09).
narrative_ontology:measurement_basis(mc_feudal_obsolescence_su_t1965, observed).
narrative_ontology:measurement(mc_feudal_obsolescence_su_t2005, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 2005, 0.07).
narrative_ontology:measurement_basis(mc_feudal_obsolescence_su_t2005, observed).
narrative_ontology:measurement(mc_feudal_obsolescence_su_t2025, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 2025, 0.06).
narrative_ontology:measurement_basis(mc_feudal_obsolescence_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_constraint_authority__feudal_obsolescence_reading, identity_coordination).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__feudal_obsolescence_reading, magna_carta_constraint_authority__living_constitutionalism_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__feudal_obsolescence_reading, magna_carta_constraint_authority__parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Magna Carta's authority' conflates three structurally distinct constraints instantiated by three readings of one kernel text. This file (feudal_obsolescence_reading) authors epsilon for the standing arrangement as the obsolescence reading assesses it: a demoted text maintained ceremonially, moderate-to-high accumulated cost, atrophied-restraint profile. The living_constitutionalism_reading authors epsilon for the same referent as an inherited due process restraint binding rulers through precedent — different victim set (the executive enters as target), different classification. The parliamentary_sovereignty_reading authors it as absorbed statute — the arrangement dissolves into parliamentary revision power. Per the epsilon-invariance principle these are three files, not one story with a measurement parameter; the upstream text's prestige feeds the downstream contest, so this file links both siblings via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
