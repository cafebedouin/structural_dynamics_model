% ============================================================================
% CONSTRAINT STORY: magna_carta_constraint_authority__parliamentary_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Magna Carta Restraints as Revisable Parliamentary Statute (Parliamentary Sovereignty Reading)
 *   domain: constitutional/legal-political
 *
 * SUMMARY:
 *   Magna Carta's restraints — due process, lawful judgment, no arbitrary
 *   taxation or detention — survive in the modern constitutional order only
 *   as absorbed into parliamentary statute law. This story instantiates the
 *   parliamentary_sovereignty_reading of the magna_carta_constraint_authority
 *   kernel: Parliament inherits the Charter's constraint authority, and
 *   because no Parliament binds its successor, every charter-derived
 *   restraint is revisable or repealable by simple majority, with the courts
 *   unable to invalidate primary legislation for departing from the Charter.
 *   The arrangement is a genuine coordination structure (lawful restraint on
 *   Crown and executive, channeled through one authoritative legislative
 *   process) that simultaneously concentrates revisionary power in the
 *   sitting majority and leaves protection for those outside that majority
 *   held at its pleasure. CONSTRAINT FAMILY NOTE (epsilon-invariance
 *   decomposition): the colloquial label 'the binding force of Magna Carta'
 *   covers three structurally distinct claims. The
 *   living_constitutionalism_reading authors lower extractiveness (judicially
 *   protected restraint, narrower victim set); the
 *   feudal_obsolescence_reading authors the original baronial compact as
 *   inert (a different referent — the 13th-century compact, not the standing
 *   statutory arrangement); this story authors the standing
 *   statutory-absorption arrangement at moderate extractiveness (0.56) as
 *   assessed by this reading's own lights. The readings are separate
 *   constraints with separate epsilon values, linked via
 *   network.affects_constraints — not one constraint with a measurement
 *   parameter.
 *
 * KEY AGENTS:
 *   - uk_parliament: agenda setter and primary beneficiary (institutional/arbitrage) — holds the inherited constraint authority; no external body can invalidate its Acts and it may revise any charter-derived provision
 *   - governing_parliamentary_majority: beneficiary (powerful/arbitrage) — exercises the revisionary power; its program meets no entrenched obstacle
 *   - the_crown: primary payer (institutional/identity_locked) — bears the restraint on prerogative; cannot revive it against statute and cannot exit the constitutional role that binds it
 *   - minorities_unprotected_by_majoritarian_legislation: secondary payer (powerless/trapped) — their protection is ordinary statute, revisable by the very majorities they must persuade
 *   - uk_judiciary: administering agent with reversal exposure (institutional/constrained) — enforces the absorbed restraints but its rulings can be reversed by ordinary statute
 *   - enfranchised_electorate: net beneficiary (organized/mobile) — popular will is mediated through Parliament; bears diffuse costs when majorities revise restraints
 *   - constitutional_scholars: analytical observer (analytical/analytical) — maps the reading contest and attests the settlement's genealogy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.56).
domain_priors:suppression_score(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.45).
domain_priors:theater_ratio(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, extractiveness, 0.56).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_constraint_authority__parliamentary_sovereignty_reading, "Magna Carta Restraints as Revisable Parliamentary Statute (Parliamentary Sovereignty Reading)").
narrative_ontology:topic_domain(magna_carta_constraint_authority__parliamentary_sovereignty_reading, "constitutional/legal-political").

domain_priors:requires_active_enforcement(magna_carta_constraint_authority__parliamentary_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 'e0e832e8-9c37-4916-9643-711a18bacae5').
narrative_ontology:cs_kernel_codification('e0e832e8-9c37-4916-9643-711a18bacae5', fixed_text).
narrative_ontology:cs_authority_grounding('e0e832e8-9c37-4916-9643-711a18bacae5', lineage).
narrative_ontology:cs_interpretation_layer_present('e0e832e8-9c37-4916-9643-711a18bacae5').
narrative_ontology:cs_reading_relation('e0e832e8-9c37-4916-9643-711a18bacae5', magna_carta_constraint_authority__living_constitutionalism_reading, coexists_with).
narrative_ontology:cs_reading_relation('e0e832e8-9c37-4916-9643-711a18bacae5', magna_carta_constraint_authority__feudal_obsolescence_reading, influences).
narrative_ontology:cs_axiom('e0e832e8-9c37-4916-9643-711a18bacae5', foundational, charter_constraint_authority_inherited_by_parliament).
narrative_ontology:cs_axiom_status(charter_constraint_authority_inherited_by_parliament, holdable).
narrative_ontology:cs_axiom_grounding('e0e832e8-9c37-4916-9643-711a18bacae5', charter_constraint_authority_inherited_by_parliament, conventional).
narrative_ontology:cs_axiom('e0e832e8-9c37-4916-9643-711a18bacae5', foundational, no_charter_provision_entrenched_against_parliamentary_revision).
narrative_ontology:cs_axiom_status(no_charter_provision_entrenched_against_parliamentary_revision, holdable).
narrative_ontology:cs_axiom_grounding('e0e832e8-9c37-4916-9643-711a18bacae5', no_charter_provision_entrenched_against_parliamentary_revision, conventional).
narrative_ontology:cs_axiom('e0e832e8-9c37-4916-9643-711a18bacae5', secondary, charter_superior_to_ordinary_statute).
narrative_ontology:cs_axiom_status(charter_superior_to_ordinary_statute, overridden).
narrative_ontology:cs_axiom_grounding('e0e832e8-9c37-4916-9643-711a18bacae5', charter_superior_to_ordinary_statute, conventional).
narrative_ontology:cs_reference_frame('e0e832e8-9c37-4916-9643-711a18bacae5', charter_restraint_absorbed_in_parliamentary_statute).
narrative_ontology:cs_drift_state('e0e832e8-9c37-4916-9643-711a18bacae5', contemporary_post_devolution, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('e0e832e8-9c37-4916-9643-711a18bacae5', '').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__parliamentary_sovereignty_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__parliamentary_sovereignty_reading, uk_parliament).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__parliamentary_sovereignty_reading, governing_parliamentary_majority).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__parliamentary_sovereignty_reading, enfranchised_electorate).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, the_crown).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, minorities_unprotected_by_majoritarian_legislation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__parliamentary_sovereignty_reading, the_crown).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, uk_judiciary).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliamentary_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__parliamentary_sovereignty_reading, implied_repeal_doctrine).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__parliamentary_sovereignty_reading, rule_of_recognition_queen_in_parliament).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The bicameral legislature at Westminster. It inherits the Charter's constraint authority through the statute book: charter-derived restraints bind as Acts of Parliament, and each new Parliament may amend or repeal any of them by ordinary majority, since no Parliament binds its successor and the courts will not hold an Act void for departing from the Charter. It maintains the restraints through legislation and collects the authority that comes with being their sole custodian. It bears no restraint it cannot itself lift.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, uk_parliament, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_constraint_authority__parliamentary_sovereignty_reading, uk_parliament, beneficiary).

% The party or coalition commanding a working majority in the Commons. Its legislative program faces no entrenched obstacle: it can revise charter-derived restraints that impede it (derogations from human-rights obligations, adjustments to judicial review, extensions of detention or protest powers) and can reverse court rulings by ordinary statute. It holds these powers only while it wins elections; losing office transfers the same powers to its opponents rather than abolishing them.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, governing_parliamentary_majority, beneficiary,
    powerful, biographical, arbitrage, national).

% The monarch and the executive acting through prerogative. Its prerogative powers — war, treaty, appointment, mercy — are bounded by statute and by the absorbed restraints: it cannot tax or detain outside parliamentary authorization, and it acts on ministerial advice answerable to Parliament. It cannot revive prerogative against an Act, and it cannot leave the constitutional role: the Crown's identity is the constitutional order that binds it. It gains continuity, legitimacy, and survival from the statutory order — a monarchy that accepted statutory restraint outlived the absolutist monarchies — but it bears the restraint and cannot revise it.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, the_crown, payer,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_constraint_authority__parliamentary_sovereignty_reading, the_crown, beneficiary).

% Groups whose legal protection against state power consists of ordinary statutes — equal-treatment provisions, due-process safeguards, protest and assembly rights — that any future majority can amend or repeal. They have no vote-blocking mechanism, no judicial strike-down of the amending Act, and no entrenched text to appeal to; their recourse is to persuade successive majorities, which is weakest precisely when a majority is inclined against them. Emigration is costly and does not secure protection for those left behind.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, minorities_unprotected_by_majoritarian_legislation, payer,
    powerless, biographical, trapped, national).

% The senior courts. They administer the absorbed restraints day to day: construing charter-derived statutes, developing common-law protections at the margins, reviewing executive action. They cannot invalidate an Act of Parliament for inconsistency with the Charter, and Parliament can reverse their rulings by ordinary statute — as with the 1965 Act retroactively reversing the Burmah Oil decision. Their interpretive authority is real and theirs to exercise; its boundaries are set elsewhere.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, uk_judiciary, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_constraint_authority__parliamentary_sovereignty_reading, uk_judiciary, payer).

% The mass of voters. Popular will reaches the constitution only as mediated through Parliament: they gain majority rule, the ability to replace governments, and the absence of unelected vetoes on legislation. They also bear the diffuse costs when majorities revise restraints, and minorities within the electorate have no protection stronger than the current majority's tolerance. Their exit is the ballot box — real, recurring, and unable to entrench anything.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, enfranchised_electorate, beneficiary,
    organized, biographical, mobile, national).

% Legal historians and constitutional theorists in and beyond the United Kingdom. They map how the Charter's force is claimed, absorbed, and revised; attest the genealogy of the settlement; and carry the comparative record of how other constitutional orders entrench or revise founding restraints. They collect nothing from the arrangement and bear none of its costs.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_constraint_authority__parliamentary_sovereignty_reading, uk_parliament).
narrative_ontology:fixing_cost_class(magna_carta_constraint_authority__parliamentary_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Channels constitutional restraint through a single authoritative legislative process: the Crown and executive are bound by charter-derived restraints (due process, lawful judgment, consent to taxation, habeas corpus), and those restraints are maintained, interpreted, and revised through one body rather than through rival authorities — courts, popular conventions, or feudal privilege. Whoever controls that body controls the restraint.
% TRANSFER_FUNCTION: Transfers constraint authority itself: from the baronial compact, through the Crown-in-Parliament, to each successive Parliament — and with it the power to define, dilute, or repeal the restraints. It also transfers protection as a legislative output: statutory rights flow to those the sitting majority protects, and unprotected exposure to those it does not. The costs fall on the Crown (bounded prerogative it cannot recover) and on minorities whose protection is revisable.
% ABSENT_VOICES: Future generations: because no Parliament binds its successor, those who will live under future majorities have no seat in today's revisions of their protections. Historically, the unenfranchised — most adults before 1918/1928, colonial subjects, the propertyless — were bound by restraints they had no hand in revising. Minority communities facing an adverse majority are present only as petitioners, with their protection on the agenda precisely when a majority moves against it.
% DISAPPEARANCE_RATIONALE: If the statutory-absorption arrangement vanished overnight — charter restraints neither binding as statute nor revisable by Parliament — the constitutional order would face an authority vacuum: rival claimants (courts asserting charter-based review, popular conventions, revived prerogative) would contest the vacancy. The current distribution of authority among Crown, Parliament, and courts — including the courts' own jurisdictional self-understanding — is organized around this settlement, as are devolution, human-rights frameworks, and executive legal accountability, all of which presuppose the parliamentary channel.
% FOUNDING_PROBLEM: The Charter's original problem was capricious royal power: arbitrary taxation, detention, and dispossession by the Crown. This reading's specific founding problem arose when the compact's feudal parties passed away: how can the Charter's restraints continue to bind a sovereign whose will is becoming parliamentary? The answer — absorb the restraints into statute so that lawful restraint survives the death of its original enforcers — is the arrangement this story classifies.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the courts' continued enforcement of habeas corpus, due-process statutes, and judicial review attests that restraint on executive power remains operative and needed; constitutional historians of the 17th-century settlement attest the absorption mechanism and its rationale; and the Crown's own conduct (acting on advice, litigating the limits of prerogative) attests the binding force. Parliamentary and governmental attestation exists but is not load-bearing for the status claim.
narrative_ontology:disappearance_verdict(magna_carta_constraint_authority__parliamentary_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_constraint_authority__parliamentary_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_constraint_authority__parliamentary_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.56, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is moderate (0.56): the restraints genuinely operate — habeas corpus, due-process statutes, consent to taxation — but their force is exactly as strong as the current majority's tolerance, making protection contingent for anyone outside that majority. Suppression (0.45) is structural rather than personally coercive: the arrangement forecloses rival constraint authorities (judicial strike-down of primary legislation, entrenchment against implied repeal) through doctrine and procedure rather than force. The enforcement series shows suppression peaking at the 17th-century settlement — foreclosure of divine-right claims and prerogative courts, revised oaths, the suppression of rival sovereignty claims — then normalizing into self-reproducing doctrine, with mild re-intensification amid devolution and human-rights-framework tensions. Theater (0.50) is high and rising across the interval: as the Charter's operative content became fully statutory, ceremonial and rhetorical veneration (Runnymede memorials, anniversary rites, constant political invocation of the Charter) grew to parity with functional restraint — the omega charter_myth_protective_function flags whether that theater does protective work. Accessibility collapse (0.65): once the absorption arrangement is understood, alternatives (an entrenched charter, judicial supremacy, popular-sovereignty constraint) are mostly foreclosed in the UK order, though devolution and Convention rights leave residual friction. Resistance (0.45): minority litigation, entrenchment campaigns, and judicial obiter questioning supremacy meet the arrangement without displacing it. Receipt surface: the arrangement's gains — the inherited authority monopoly and the revisionary power itself — accrue to the uk_parliament seat, which is why gain_flow names it; the governing majority exercises the power between elections, but the institution holds it across them. Fixing cost: entrenching the restraints against future majorities would require the only actor who could fix it (Parliament) to bind itself, or courts to develop common-law entrenchment against the orthodox doctrine — a cost to the fixer that exceeds the benefit it would bear, hence prohibitive. All series run on one shared time grid (1297, 1354, 1628, 1689, 1832, 1911, 1965, 2025) with every tracked metric authored at every point.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute different types from the same structure. From the uk_parliament and governing_parliamentary_majority seats the arrangement is self-government and lawful continuity — coordination they administer and profit from. From the the_crown seat the same structure is subordination: its prerogative is bounded by a rival authority it cannot revise. From the minorities_unprotected_by_majoritarian_legislation seat it is protection held at the majority's pleasure — a restraint that exists but guarantees nothing. The uk_judiciary seat straddles: it administers the restraint while bearing the standing knowledge that any ruling can be reversed by ordinary statute. The authored claim (tangled_rope) does not adjudicate between these; the engine computes per-seat classifications from power, exit, and role data.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low directionality: uk_parliament and governing_parliamentary_majority hold arbitrage-grade control — they can rewrite the constraint itself — placing them nearest the beneficiary end. enfranchised_electorate sits low-moderate: it receives mediated popular will but bears diffuse costs when majorities revise restraints. Victim declarations map to high directionality: the_crown is the original target of the Charter's restraints — the restraint binds it, it cannot revise it, and it is identity-locked to the constitutional order it inhabits — so it sits near the full-target end despite its incidental benefit from constitutional continuity (declared via secondary_role, not by moving it out of the victim set). minorities_unprotected_by_majoritarian_legislation are trapped: their only recourse is persuading successive majorities, so they sit near the full-target end with no exit modulation. The uk_judiciary is near-symmetric — it collects interpretive authority from administering the restraints and pays reversal risk; its dual position is declared through secondary_role rather than a directionality override, because overrides key on the power atom and would also move uk_parliament and the_crown, which the coarse key cannot distinguish.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim prevents two opposite misreadings. Read as pure rope, the revisionary power disappears — but that power is the structure: the same legislative channel that maintains lawful restraint can dissolve any particular restraint, which is precisely the asymmetric extraction the tangled_rope category exists to name. Read as snare, the genuine coordination function vanishes — but the restraints do bind the executive, do protect due process, and are not maintained solely to suppress exits. On mandatrophy proper: the kernel's original mandate (a baronial compact binding the Crown directly, enforced by feudal parties) is dead — its enforcers and context are gone, and that death is exactly why the readings contest. But THIS reading's arrangement has a live mandate: capricious executive power remains a real problem the arrangement addresses, so founding_problem_status is live and the mismatch consumer should not flag capture. The piton-side risk is monitored through the theater series rather than assumed — theater_ratio at 0.50 is the symptom to watch, while the cost-asymmetry test (Parliament could fix it but bears little of the extraction) is what keeps this from computing as an inertial remnant.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_status,
    'Is the parliamentary-sovereignty reading the correct instantiation of the magna_carta_constraint_authority kernel, or do the living-constitutionalism reading (restraint binding through juridical precedent) or the feudal-obsolescence reading (no binding authority over modern sovereignty) better capture the Charter''s operative force?',
    'Doctrinal development: a court asserting charter-based review of primary legislation, a formal entrenchment enacted and upheld against implied repeal, or a constitutional moment (a written constitution) would shift the kernel''s dominant reading; comparative analysis of how the Charter''s force is actually invoked in litigation and legislation.',
    'Under the living-constitutionalism reading the victim set narrows (judicially protected minorities) and extractiveness falls; under the feudal-obsolescence reading the inherited-authority claim dissolves and the arrangement reduces to ordinary statute-making with no kernel constraint at all. This story''s tangled_rope classification holds only within the parliamentary-sovereignty reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_status, conceptual, 'Which reading of the Magna Carta kernel correctly instantiates the constraint.').

omega_variable(
    entrenchment_possibility,
    'Is Parliament''s revisionary power over charter-derived restraints absolute, or are there limits — common-law fundamental rights, the devolution settlements, treaty-based frameworks — that even a determined majority cannot lawfully revise?',
    'A case in which a UK court refuses to apply a later Act inconsistent with a claimed entrenchment, or an entrenchment clause enacted by Parliament that survives a direct repeal attempt against the orthodox implied-repeal doctrine.',
    'If revisionary power is absolute, minority exposure is structural and the constraint sits at the extractive edge of tangled_rope; any enforceable limit converts part of the restraint into genuine rope with real bite and shrinks the minority victim set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(entrenchment_possibility, empirical, 'Whether the revisionary power over charter restraints is absolute or limited.').

omega_variable(
    revisionary_power_exercise_rate,
    'How often and how harmfully do parliamentary majorities actually exercise the revisionary power over charter-derived restraints — is the burden on unprotected groups latent or operative?',
    'Legislative-history audit of amendments and repeals of charter-derived restraints (habeas corpus derogations, detention powers, protest restrictions, human-rights-framework reform proposals), weighted by the affected groups'' lack of alternative protection.',
    'A mostly latent power supports computing the constraint near rope (coordination with modest extraction); an actively exercised power pushes the minority seats toward snare-side dynamics and raises effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revisionary_power_exercise_rate, empirical, 'Whether the revisionary power is latent or actively exercised against unprotected groups.').

omega_variable(
    charter_myth_protective_function,
    'Does the theatrical veneration of the Charter (anniversaries, memorials, rhetorical invocation) perform protective work by raising the political cost of revision, or is it pure performance with no restraint effect?',
    'Compare revision episodes where Charter rhetoric was mobilized against a repeal (did it change outcomes?) against episodes where it was absent; measure the political cost paid by governments that revised charter-derived restraints.',
    'If the myth raises revision costs, the theater_ratio overstates decay — part of the theatrical activity is functional restraint and the arrangement is more rope-like than the metric suggests; if not, theater is decay and the piton-side drift risk is real.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(charter_myth_protective_function, conceptual, 'Whether Charter veneration is functional restraint or pure theater.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 1297, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magna_carta_parl_sov_tr_t1297, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 1297, 0.12).
narrative_ontology:measurement_basis(magna_carta_parl_sov_tr_t1297, observed).
narrative_ontology:measurement(magna_carta_parl_sov_tr_t1354, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 1354, 0.15).
narrative_ontology:measurement_basis(magna_carta_parl_sov_tr_t1354, observed).
narrative_ontology:measurement(magna_carta_parl_sov_tr_t1628, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 1628, 0.28).
narrative_ontology:measurement_basis(magna_carta_parl_sov_tr_t1628, observed).
narrative_ontology:measurement(magna_carta_parl_sov_tr_t1689, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 1689, 0.3).
narrative_ontology:measurement_basis(magna_carta_parl_sov_tr_t1689, observed).
narrative_ontology:measurement(magna_carta_parl_sov_tr_t1832, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 1832, 0.38).
narrative_ontology:measurement_basis(magna_carta_parl_sov_tr_t1832, observed).
narrative_ontology:measurement(magna_carta_parl_sov_tr_t1911, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 1911, 0.42).
narrative_ontology:measurement_basis(magna_carta_parl_sov_tr_t1911, observed).
narrative_ontology:measurement(magna_carta_parl_sov_tr_t1965, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 1965, 0.47).
narrative_ontology:measurement_basis(magna_carta_parl_sov_tr_t1965, observed).
narrative_ontology:measurement(magna_carta_parl_sov_tr_t2025, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 2025, 0.5).
narrative_ontology:measurement_basis(magna_carta_parl_sov_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(magna_carta_parl_sov_be_t1297, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 1297, 0.3).
narrative_ontology:measurement_basis(magna_carta_parl_sov_be_t1297, observed).
narrative_ontology:measurement(magna_carta_parl_sov_be_t1354, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 1354, 0.31).
narrative_ontology:measurement_basis(magna_carta_parl_sov_be_t1354, observed).
narrative_ontology:measurement(magna_carta_parl_sov_be_t1628, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 1628, 0.42).
narrative_ontology:measurement_basis(magna_carta_parl_sov_be_t1628, observed).
narrative_ontology:measurement(magna_carta_parl_sov_be_t1689, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 1689, 0.5).
narrative_ontology:measurement_basis(magna_carta_parl_sov_be_t1689, observed).
narrative_ontology:measurement(magna_carta_parl_sov_be_t1832, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 1832, 0.5).
narrative_ontology:measurement_basis(magna_carta_parl_sov_be_t1832, observed).
narrative_ontology:measurement(magna_carta_parl_sov_be_t1911, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 1911, 0.54).
narrative_ontology:measurement_basis(magna_carta_parl_sov_be_t1911, observed).
narrative_ontology:measurement(magna_carta_parl_sov_be_t1965, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 1965, 0.58).
narrative_ontology:measurement_basis(magna_carta_parl_sov_be_t1965, observed).
narrative_ontology:measurement(magna_carta_parl_sov_be_t2025, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 2025, 0.56).
narrative_ontology:measurement_basis(magna_carta_parl_sov_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(magna_carta_parl_sov_su_t1297, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1297, 0.25).
narrative_ontology:measurement_basis(magna_carta_parl_sov_su_t1297, observed).
narrative_ontology:measurement(magna_carta_parl_sov_su_t1354, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1354, 0.26).
narrative_ontology:measurement_basis(magna_carta_parl_sov_su_t1354, observed).
narrative_ontology:measurement(magna_carta_parl_sov_su_t1628, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1628, 0.52).
narrative_ontology:measurement_basis(magna_carta_parl_sov_su_t1628, observed).
narrative_ontology:measurement(magna_carta_parl_sov_su_t1689, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1689, 0.6).
narrative_ontology:measurement_basis(magna_carta_parl_sov_su_t1689, observed).
narrative_ontology:measurement(magna_carta_parl_sov_su_t1832, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1832, 0.5).
narrative_ontology:measurement_basis(magna_carta_parl_sov_su_t1832, observed).
narrative_ontology:measurement(magna_carta_parl_sov_su_t1911, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1911, 0.45).
narrative_ontology:measurement_basis(magna_carta_parl_sov_su_t1911, observed).
narrative_ontology:measurement(magna_carta_parl_sov_su_t1965, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1965, 0.4).
narrative_ontology:measurement_basis(magna_carta_parl_sov_su_t1965, observed).
narrative_ontology:measurement(magna_carta_parl_sov_su_t2025, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 2025, 0.45).
narrative_ontology:measurement_basis(magna_carta_parl_sov_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_constraint_authority__parliamentary_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__parliamentary_sovereignty_reading, magna_carta_constraint_authority__living_constitutionalism_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__parliamentary_sovereignty_reading, magna_carta_constraint_authority__feudal_obsolescence_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the kernel 'Magna Carta's binding force' decomposes into three readings per the epsilon-invariance principle, because the label conflates structurally distinct claims with distinct epsilon values and victim sets. This story (parliamentary_sovereignty_reading) authors the standing statutory-absorption arrangement at moderate extractiveness. The living_constitutionalism_reading authors judicially protected restraint (lower epsilon, narrower victim set); the feudal_obsolescence_reading authors the original compact as inert (different referent entirely). The upstream reading with the strongest institutional grip is this one — it is the operative settlement — and its operation (absorption of the Charter into revisable statute) changes the legitimacy conditions of both siblings: it feeds the obsolescence reading's claim about the original text while setting the statutory limits within which the living-constitutionalism reading can operate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
