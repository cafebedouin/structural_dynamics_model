% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__republican_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_legitimacy__republican_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: sovereign_legitimacy__republican_reading
 *   human_readable: Republican Reading of Sovereign Legitimacy — Authority Flowing Upward Through Delegated Consent
 *   domain: political philosophy/constitutional theory/legitimacy studies
 *
 * SUMMARY:
 *   The republican reading of sovereign legitimacy holds that political
 *   authority is legitimate only when it flows upward from the people through
 *   delegated consent, validated by ongoing electoral renewal and bounded by
 *   constitutional adherence. As a standing arrangement, it solves the
 *   post-divine-right legitimation problem while generating its own
 *   asymmetries: the franchise boundary determines who counts as consenting,
 *   and everyone else — disenfranchised residents, persistent minorities,
 *   future generations, foreign subjects of republican power — bears the
 *   arrangement's obligations without a seat in the consenting public. The
 *   claim/metric split is deliberate: the reading CLAIMS itself as the
 *   accountability arrangement par excellence, while the authored metrics
 *   describe a moderately extractive, actively enforced structure whose
 *   validation ritual is slowly ritualizing. The engine measures that
 *   divergence per seat; nothing here reconciles claim to metric. KEY AGENTS
 *   (by structural relationship): - enfranchised_citizenry: principal
 *   beneficiary (organized/constrained) — delegates authority, bears civic
 *   obligation - elected_officeholders: agenda-setter and beneficiary
 *   (institutional/mobile) — exercises delegated power under renewal
 *   discipline - constitutional_courts: agenda-setter
 *   (institutional/constrained) — polices the consent procedure's boundaries
 *   - political_parties: beneficiary and administrator (institutional/mobile)
 *   — intermediates consent, controls access - dissenting_minorities:
 *   recurring loser (moderate/identity_locked) — bound by majorities formed
 *   against them - disenfranchised_residents: bound non-consenter
 *   (powerless/trapped) — full obligation, zero voice - future_generations:
 *   bound successor (powerless/trapped) — consents only retrospectively -
 *   foreign_subjects_of_republican_power: excluded party (powerless/trapped)
 *   — affected without any channel - legitimacy_scholars: analytical observer
 *   (analytical/analytical) — sees the full structure from outside
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__republican_reading, 0.45).
domain_priors:suppression_score(sovereign_legitimacy__republican_reading, 0.44).
domain_priors:theater_ratio(sovereign_legitimacy__republican_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, accessibility_collapse, 0.28).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__republican_reading, tangled_rope).
narrative_ontology:human_readable(sovereign_legitimacy__republican_reading, "Republican Reading of Sovereign Legitimacy — Authority Flowing Upward Through Delegated Consent").
narrative_ontology:topic_domain(sovereign_legitimacy__republican_reading, "political philosophy/constitutional theory/legitimacy studies").

domain_priors:requires_active_enforcement(sovereign_legitimacy__republican_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__republican_reading, '0622f8be-d6ee-4a84-855c-c86e9430c654').
narrative_ontology:cs_kernel_codification('0622f8be-d6ee-4a84-855c-c86e9430c654', formalized).
narrative_ontology:cs_authority_grounding('0622f8be-d6ee-4a84-855c-c86e9430c654', practice).
narrative_ontology:cs_interpretation_layer_present('0622f8be-d6ee-4a84-855c-c86e9430c654').
narrative_ontology:cs_reading_relation('0622f8be-d6ee-4a84-855c-c86e9430c654', sovereign_legitimacy__monarchical_reading, forecloses).
narrative_ontology:cs_reading_relation('0622f8be-d6ee-4a84-855c-c86e9430c654', sovereign_legitimacy__constitutional_hybrid_reading, forecloses).
narrative_ontology:cs_axiom('0622f8be-d6ee-4a84-855c-c86e9430c654', foundational, popular_sovereignty_exclusive_source).
narrative_ontology:cs_axiom_status(popular_sovereignty_exclusive_source, holdable).
narrative_ontology:cs_axiom_grounding('0622f8be-d6ee-4a84-855c-c86e9430c654', popular_sovereignty_exclusive_source, deontological).
narrative_ontology:cs_axiom('0622f8be-d6ee-4a84-855c-c86e9430c654', secondary, delegated_authority_requires_periodic_renewal).
narrative_ontology:cs_axiom_status(delegated_authority_requires_periodic_renewal, holdable).
narrative_ontology:cs_axiom_grounding('0622f8be-d6ee-4a84-855c-c86e9430c654', delegated_authority_requires_periodic_renewal, instrumental).
narrative_ontology:cs_reference_frame('0622f8be-d6ee-4a84-855c-c86e9430c654', popular_constituent_supremacy).
narrative_ontology:cs_drift_state('0622f8be-d6ee-4a84-855c-c86e9430c654', contemporary_mass_electorate, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0622f8be-d6ee-4a84-855c-c86e9430c654', '').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__republican_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__republican_reading, enfranchised_citizenry).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__republican_reading, elected_officeholders).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__republican_reading, political_parties).
narrative_ontology:constraint_victim(sovereign_legitimacy__republican_reading, disenfranchised_residents).
narrative_ontology:constraint_victim(sovereign_legitimacy__republican_reading, dissenting_minorities).
narrative_ontology:constraint_victim(sovereign_legitimacy__republican_reading, future_generations).
narrative_ontology:constraint_victim(sovereign_legitimacy__republican_reading, foreign_subjects_of_republican_power).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(sovereign_legitimacy__republican_reading, enfranchised_citizenry).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Votes in periodic elections, serves on juries, pays taxes, and complies with statutes passed by representatives they may have opposed. They can vote a government out at the next cycle but cannot opt out of its obligations between cycles. Emigration exists but severs home, family, livelihood, and usually citizenship rights accumulated over a lifetime.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, enfranchised_citizenry, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__republican_reading, enfranchised_citizenry, payer).

% Exercises delegated decision-making authority for fixed terms. Calls and shapes elections, proposes constitutional amendments, appoints judges, and commands the administrative apparatus that the arrangement's obligations fund. Their titles lapse unless renewed at the ballot box; leaving office returns them to private life with pensions, networks, and reputational capital intact.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, elected_officeholders, agenda_setter,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__republican_reading, elected_officeholders, beneficiary).

% Adjudicates what the consent procedure itself permits: ballot access, districting, amendment limits, emergency powers. Judges hold insulated tenure and resolve disputes case by case, absorbing pressures on the founding document through interpretation rather than facing them as open revision. Removal of a court is itself a constitutional crisis.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, constitutional_courts, agenda_setter,
    institutional, generational, constrained, national).

% Selects and finances candidates, controls primaries and ballot access, and converts dispersed votes into governing coalitions. Organizational survival depends on the electoral cycle continuing; parties intermediate between individual voters and public office, collecting staffing, patronage, and agenda-setting capacity along the way.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, political_parties, beneficiary,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__republican_reading, political_parties, agenda_setter).

% Participates fully and loses repeatedly: religious, linguistic, and regional minorities bound by majorities routinely formed against them. Their self-understanding is constituted through membership in the polity, so exit would mean abandoning community, language institutions, and identity itself. They stay and litigate, protest, or endure the outcomes.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, dissenting_minorities, payer,
    moderate, biographical, identity_locked, national).

% Lives under the laws, pays taxes, is policed, and in some jurisdictions is subject to conscription, but holds no vote: resident non-citizens, criminally disenfranchised persons, and others the franchise boundary excludes. Obligations attach immediately upon presence; pathways to voice (naturalization, rights restoration) are long, discretionary, and revocable.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, disenfranchised_residents, payer,
    powerless, biographical, trapped, national).

% Inherits public debt, entrenched constitutional provisions, and long-horizon environmental and fiscal commitments made by predecessors. They hold no seat in any current election; their consent is presumed retroactively once they arrive, and the entrenchment clauses they inherit are often designed to be difficult for them to revise.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, future_generations, payer,
    powerless, generational, trapped, national).

% Affected by wars, sanctions, trade rules, basing decisions, and development policies made by republican governments in which they hold no vote and to which they owe no allegiance. No procedural channel exists for their objection inside the consenting public; their recourse is limited to diplomacy, resistance, or exit from regions of influence.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, foreign_subjects_of_republican_power, excluded,
    powerless, biographical, trapped, global).

% Traces the doctrine from Locke and Rousseau through ratification debates to contemporary constitutional design. Maps where consent is actual, presumed, or fictional, and compares validation mechanisms across regimes. Holds no stake in any particular polity's arrangement and can see the full structure from outside it.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, legitimacy_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of authorizing coercive political power without appeal to divine right or bloodline: it provides a peaceful, repeatable procedure for installing and removing rulers (elections), a shared standard for when obedience is owed (delegated consent), and a bounded term structure that caps any holder's tenure.
% TRANSFER_FUNCTION: Moves decision-making authority from the citizen body to elected officeholders for fixed renewable terms; moves political obligation — compliance, taxation, jury service, military service where applicable — from all governed persons to sustain institutions that only some of them had any hand in authorizing.
% ABSENT_VOICES: Foreign subjects of republican power would object that wars, sanctions, and trade regimes bind them without any franchise; disenfranchised residents inside the polity would object to full obligation with zero voice; future generations would object to entrenchment clauses they cannot yet reach. All three sit outside the electoral conversation by design — the consent mechanism defines its own boundary of who counts as consenting, and each of these groups falls on the wrong side of a line none of them drew.
% DISAPPEARANCE_RATIONALE: If the upward-delegation principle vanished overnight, every republic's claim to obedience would collapse simultaneously: officeholders would hold titles with no authorization story, tax collection and conscription would lose their justification, and the vacuum would be filled either by revived inheritance claims, by dual-sourced compromises, or by raw coercion. Constitutional orders worldwide would need to re-found themselves on some other legitimacy source within months.
% FOUNDING_PROBLEM: How to make coercive rule legitimate once divine-right and hereditary foundations lost credibility — the post-revolutionary legitimation crisis of grounding political authority in human agreement rather than cosmic order.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: rival traditions (monarchical and hybrid readings) address the same legitimation problem by different means, conceding its persistence; international self-determination law presupposes it; post-colonial constitution-making consistently reaches for popular-sovereignty formulas; and the disenfranchised attest the problem is live while disputing whether this arrangement solves it for them. No serious contemporary school claims the problem is dead.
narrative_ontology:disappearance_verdict(sovereign_legitimacy__republican_reading, world_rearranges).
narrative_ontology:founding_problem_status(sovereign_legitimacy__republican_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__republican_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sovereign_legitimacy__republican_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sovereign_legitimacy__republican_reading, 0.45, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_legitimacy__republican_reading_tests).
:- end_tests(sovereign_legitimacy__republican_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45 at interval end) because the arrangement genuinely returns accountability — removal is real, terms are bounded — while still imposing full obligation on persons the consent mechanism never reached. The temporal series shows the honest arc: extraction starts high (0.72) in the founding era, when proclaimed popular sovereignty coexisted with property qualifications, chattel slavery, and total female exclusion; franchise expansion drives it down through the middle of the interval; the curve flattens (0.47 to 0.45) as the easy expansions exhaust and the remaining excluded — non-citizens, the criminally disenfranchised, the unborn, the foreign — are excluded by categories the winning majorities have little incentive to widen. Theater ratio rises steadily (0.12 to 0.30): elections began as genuinely decisive contests among a small enfranchised class and have progressively acquired ritual components — plebiscitary spectacle, money-mediated messaging, turnout as performance — a slow Goodhart drift in which the validation ceremony partially substitutes for the validation function. Suppression requirement climbs gently (0.32 to 0.44): narrow early franchises needed little enforcement machinery because consent was cheap to manufacture among the few; mass electorates, professionalized administration, and polarized losers require standing enforcement infrastructure to make outcomes stick. Suppression is authored as a raw structural property and is deliberately left unscaled — scaling by directionality and scope is the engine's arithmetic, not the author's. All three series share one time grid (points 0–120 step 20) so no metric is sampled against another's end-state.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary and agenda-setter seats should compute a fundamentally different type than the payer seats. From the officeholder's chair, the arrangement is the operating condition of their own authority: it confers legitimacy, structures their tenure, and disciplines them only at intervals they can prepare for. From the disenfranchised resident's chair, the identical structure is binding obligation with no corresponding voice — every statute, tax, and policing decision arrives pre-authorized by someone else's consent. The dissenting minority experiences the same election as both ritual and ratification-of-defeat. The constitutional court sees neither face; it sees the procedure itself. These divergences fall out of the authored power, exit, and role data — the engine computes them; the prose here only explains why they must appear.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the enfranchised citizenry, officeholders, and parties toward the beneficiary end of the directionality axis: the citizenry is organized but exit-constrained (their benefit is real but they also pay — hence the secondary payer role), officeholders are institutional with mobile exit (terms end, careers continue), and parties are institutional arbiters of access whose existence depends on the cycle continuing. Victim declarations drive the four payer seats toward the target end: disenfranchised residents and future generations are powerless and trapped (maximum amplification), dissenting minorities are moderate-powered but identity_locked (trapped by identity rather than law — they could physically leave but not without dissolving the community their selfhood is made of), and foreign subjects are powerless, globally scoped, and wholly outside the consenting set. No directionality overrides are authored: the beneficiary/victim declarations plus exit atoms already separate every seat correctly, and the one genuinely mixed agent (officeholders, who benefit from delegation but submit to removal) is captured by the secondary-role structure rather than by overriding the derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite misreadings. Calling this a snare would erase the real coordination: the arrangement genuinely solved the legitimation problem, genuinely removes rulers peacefully, and genuinely bounds tenure — the coordination story is not cover. Calling it a rope would erase the victims: four identifiable groups bear its full costs without entering the consent that authorizes it, and holding the structure together requires active enforcement (courts, electoral administration, police power behind obligation). The tangled_rope claim keeps both faces visible simultaneously. On obsolescence: the founding problem — legitimating authority without divine right — is live, and the corroboration is external (rival traditions address the same problem; international law presupposes it; the disenfranchised confirm it while disputing the solution). Because the problem is live and the validation mechanism still functions, the arrangement is not inertial performance; the rising theater ratio is a drift signal worth watching, not yet a piton signature. The mismatch consumer should read founding_problem_status=live against disappearance_verdict=world_rearranges and find them consistent: the world would rearrange because the problem the arrangement solves is still unsolved by anything else.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the sovereign_legitimacy kernel: does legitimate authority flow upward from the people (this reading), downward from an inheriting sovereign (monarchical_reading), or dual-source through constitutional mediation (constitutional_hybrid_reading)? Where exactly do the readings diverge?',
    'Conceptual, not empirical: the disagreement is located at the source-of-authority premise — whether any authority independent of popular consent can be legitimate. It resolves only by which framework a party adopts; comparative constitutional history documents the consequences of each adoption but cannot adjudicate the premise itself.',
    'Adopting monarchical_reading replaces the victim set (subjects of inherited rule replace the disenfranchised) and swaps the validation mechanism (succession replaces electoral cycles); adopting constitutional_hybrid_reading splits the beneficiary set between ceremonial and political tracks. Each sibling is a separate constraint with its own epsilon, not a variant measurement of this one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: this story instantiates the republican reading of a three-way-contested legitimacy kernel.').

omega_variable(
    franchise_boundary_definition,
    'Who counts as ''the people'' whose consent grounds authority? Every republic draws the boundary — by citizenship, age, criminal status, territory — and the consent mechanism defines its own constituency.',
    'Comparative analysis of franchise law, naturalization regimes, and disenfranchisement practices across consolidated republics, tracked against each polity''s own professed principles.',
    'A narrower boundary than the rhetoric implies raises effective extraction on the excluded seats substantially; a boundary matching the rhetoric supports the moderate-extraction reading. The victim set''s size is a direct input to per-seat classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(franchise_boundary_definition, conceptual, 'The constituency of consent is defined by the arrangement it legitimates — circularity at the heart of the reading.').

omega_variable(
    hypothetical_consent_fiction,
    'Is tacit or presumed consent — voting as consent, residence as consent, non-emigration as consent — genuine consent, or a fiction covering obligation imposed without agreement?',
    'Behavioral and historical evidence on whether participation tracks endorsement or resignation: turnout among the indifferent, exit-cost studies, and the Humean observation that consent inferred from submission would justify any stable regime.',
    'If consent is largely fictional, the upward-flow premise weakens for every non-consenting governed person and effective extraction rises sharply toward the extraction-dominant profile; if participation genuinely tracks endorsement, the moderate reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hypothetical_consent_fiction, conceptual, 'Whether the social contract''s consent is actual or constructed — the oldest internal objection to the reading.').

omega_variable(
    majoritarian_tyranny_exposure,
    'Does the arrangement adequately protect persistent minorities, or does delegated majority consent systematically license domination of those who lose every cycle?',
    'Comparative constitutional performance data: minority rights outcomes, court-protection effectiveness, and anti-majoritarian institution durability across consolidated republics over multiple decades.',
    'Systematic failure shifts the arrangement toward an extraction-dominant profile for the dissenting_minorities seat specifically, even where the aggregate profile stays moderate; robust protection supports the accountability reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majoritarian_tyranny_exposure, empirical, 'Whether electoral accountability for majorities translates into protection for permanent losers.').

omega_variable(
    intergenerational_binding_legitimacy,
    'Can one generation legitimately bind later ones through entrenched constitutions, debt, and long-horizon commitments those later generations never consented to?',
    'Constitutional-amendment jurisprudence and democratic theory on entrenchment limits, plus empirical study of how successfully each new generation revises inherited commitments in practice.',
    'If intergenerational binding is illegitimate, the future_generations seat''s burden is imposed rather than contracted, deepening its target-side directionality and raising overall extraction; if binding is a legitimate exercise of constituent power, the burden reads as inherited membership.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_binding_legitimacy, conceptual, 'Whether presumed retrospective consent of successors can ground obligations imposed before they existed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__republican_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sove_tr_t0, sovereign_legitimacy__republican_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(sove_tr_t20, sovereign_legitimacy__republican_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(sove_tr_t40, sovereign_legitimacy__republican_reading, theater_ratio, 40, 0.19).
narrative_ontology:measurement(sove_tr_t60, sovereign_legitimacy__republican_reading, theater_ratio, 60, 0.22).
narrative_ontology:measurement(sove_tr_t80, sovereign_legitimacy__republican_reading, theater_ratio, 80, 0.26).
narrative_ontology:measurement(sove_tr_t100, sovereign_legitimacy__republican_reading, theater_ratio, 100, 0.28).
narrative_ontology:measurement(sove_tr_t120, sovereign_legitimacy__republican_reading, theater_ratio, 120, 0.3).

% Extraction over time
narrative_ontology:measurement(sove_be_t0, sovereign_legitimacy__republican_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(sove_be_t20, sovereign_legitimacy__republican_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(sove_be_t40, sovereign_legitimacy__republican_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(sove_be_t60, sovereign_legitimacy__republican_reading, base_extractiveness, 60, 0.52).
narrative_ontology:measurement(sove_be_t80, sovereign_legitimacy__republican_reading, base_extractiveness, 80, 0.47).
narrative_ontology:measurement(sove_be_t100, sovereign_legitimacy__republican_reading, base_extractiveness, 100, 0.46).
narrative_ontology:measurement(sove_be_t120, sovereign_legitimacy__republican_reading, base_extractiveness, 120, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(sove_su_t0, sovereign_legitimacy__republican_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement(sove_su_t20, sovereign_legitimacy__republican_reading, suppression_requirement, 20, 0.34).
narrative_ontology:measurement(sove_su_t40, sovereign_legitimacy__republican_reading, suppression_requirement, 40, 0.37).
narrative_ontology:measurement(sove_su_t60, sovereign_legitimacy__republican_reading, suppression_requirement, 60, 0.39).
narrative_ontology:measurement(sove_su_t80, sovereign_legitimacy__republican_reading, suppression_requirement, 80, 0.41).
narrative_ontology:measurement(sove_su_t100, sovereign_legitimacy__republican_reading, suppression_requirement, 100, 0.43).
narrative_ontology:measurement(sove_su_t120, sovereign_legitimacy__republican_reading, suppression_requirement, 120, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_legitimacy__republican_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(sovereign_legitimacy__republican_reading, monarchical_reading).
narrative_ontology:affects_constraint(sovereign_legitimacy__republican_reading, constitutional_hybrid_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the sovereign_legitimacy kernel. The colloquial question 'who may legitimately rule?' covers three structurally distinct claims with different validation mechanisms, different beneficiary/victim sets, and different epsilon values: this file instantiates the republican reading (upward flow, electoral validation, franchise-bounded beneficiary set); monarchical_reading instantiates downward flow through inherited right (validation by succession; victims are subjects rather than the disenfranchised); constitutional_hybrid_reading instantiates dual sourcing mediated by constitutional law. Each is authored as a separate story with its own stable epsilon per the epsilon-invariance principle; edges here record the family linkage.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
