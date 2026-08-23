% ============================================================================
% CONSTRAINT STORY: constitutional_text__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text__judicial_supremacy_reading, []).

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
 *   constraint_id: constitutional_text__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy Reading of Constitutional Text: Courts as Final Interpreters
 *   domain: constitutional theory/political philosophy/comparative law
 *
 * SUMMARY:
 *   In constitutional orders of the American type, the constitutional text is
 *   read to vest final interpretive authority in the courts: when the apex
 *   court holds a statute unconstitutional, that holding is the conclusive
 *   determination of constitutional meaning, and no ordinary legislative act
 *   can displace it short of amendment. This story instantiates the
 *   judicial_supremacy_reading of the contested kernel constitutional_text;
 *   the sibling readings — legislative_sovereignty_reading
 *   (override/notwithstanding designs) and popular_sovereignty_reading
 *   (constituent-power retention) — are separate constraints with their own
 *   files, linked through network.affects_constraints. Per the
 *   epsilon-referent rule, epsilon here is authored for the standing
 *   judicial-supremacy arrangement as THIS reading assesses it: the reading
 *   affirms the arrangement's settlement and rights-guardianship functions
 *   while acknowledging that the same structure forecloses legislative
 *   finality and filters democratic preference formation — hence a moderate
 *   epsilon, neither the near-zero a devoted defender would report nor the
 *   near-one a popular-sovereignty sibling would assign to the same referent.
 *   The claim/metric split is deliberate: claimed_type states the structure
 *   (both a genuine coordination function and asymmetric, enforced extraction
 *   are present); the metrics describe observed operation independently. KEY
 *   AGENTS (by structural relationship): - constitutional_judiciary:
 *   Agenda-setter and primary beneficiary (institutional/identity_locked) —
 *   holds final interpretive authority; converts decided controversies into
 *   doctrinal ownership - rights_claimant_minorities: Primary intended
 *   beneficiary (powerless/trapped) — obtains protection unavailable through
 *   ordinary politics - legislative_majorities: Primary target
 *   (powerful/constrained) — elected bodies whose enactments are voidable
 *   without legislative recourse - majoritarian_voters: Diffuse target with
 *   secondary benefit (moderate/constrained) — electoral preferences filtered
 *   through judicial review - lower_courts: Secondary beneficiary
 *   (institutional/constrained) — receives settled doctrine, spared ultimate
 *   interpretive responsibility - direct_democracy_advocates: Excluded party
 *   (organized/trapped) — proposes referendum/convention forums the
 *   arrangement does not contain - comparative_constitutional_scholars:
 *   Analytical observer (analytical/analytical) — maps cross-system variation
 *   in final-authority allocation
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__judicial_supremacy_reading, 0.66).
domain_priors:suppression_score(constitutional_text__judicial_supremacy_reading, 0.7).
domain_priors:theater_ratio(constitutional_text__judicial_supremacy_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text__judicial_supremacy_reading, "Judicial Supremacy Reading of Constitutional Text: Courts as Final Interpreters").
narrative_ontology:topic_domain(constitutional_text__judicial_supremacy_reading, "constitutional theory/political philosophy/comparative law").

domain_priors:requires_active_enforcement(constitutional_text__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__judicial_supremacy_reading, '3c2281cc-b083-4b02-a791-5a15e9f95c30').
narrative_ontology:cs_kernel_codification('3c2281cc-b083-4b02-a791-5a15e9f95c30', fixed_text).
narrative_ontology:cs_authority_grounding('3c2281cc-b083-4b02-a791-5a15e9f95c30', lineage).
narrative_ontology:cs_interpretation_layer_present('3c2281cc-b083-4b02-a791-5a15e9f95c30').
narrative_ontology:cs_reading_relation('3c2281cc-b083-4b02-a791-5a15e9f95c30', constitutional_text__legislative_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('3c2281cc-b083-4b02-a791-5a15e9f95c30', constitutional_text__popular_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('3c2281cc-b083-4b02-a791-5a15e9f95c30', foundational, judicial_invalidation_is_conclusive_meaning).
narrative_ontology:cs_axiom_status(judicial_invalidation_is_conclusive_meaning, holdable).
narrative_ontology:cs_axiom_grounding('3c2281cc-b083-4b02-a791-5a15e9f95c30', judicial_invalidation_is_conclusive_meaning, conventional).
narrative_ontology:cs_axiom('3c2281cc-b083-4b02-a791-5a15e9f95c30', foundational, countermajoritarian_rights_guardianship).
narrative_ontology:cs_axiom_status(countermajoritarian_rights_guardianship, holdable).
narrative_ontology:cs_axiom_grounding('3c2281cc-b083-4b02-a791-5a15e9f95c30', countermajoritarian_rights_guardianship, deontological).
narrative_ontology:cs_reference_frame('3c2281cc-b083-4b02-a791-5a15e9f95c30', text_granted_judicial_finality).
narrative_ontology:cs_drift_state('3c2281cc-b083-4b02-a791-5a15e9f95c30', contemporary_court_curbing_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3c2281cc-b083-4b02-a791-5a15e9f95c30', '').
narrative_ontology:cs_kernel_id(constitutional_text__judicial_supremacy_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__judicial_supremacy_reading, rights_claimant_minorities).
narrative_ontology:constraint_beneficiary(constitutional_text__judicial_supremacy_reading, constitutional_judiciary).
narrative_ontology:constraint_beneficiary(constitutional_text__judicial_supremacy_reading, lower_courts).
narrative_ontology:constraint_victim(constitutional_text__judicial_supremacy_reading, legislative_majorities).
narrative_ontology:constraint_victim(constitutional_text__judicial_supremacy_reading, majoritarian_voters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_text__judicial_supremacy_reading, majoritarian_voters).
narrative_ontology:constraint_vindicates(constitutional_text__judicial_supremacy_reading, judicial_finality_doctrine).
narrative_ontology:constraint_vindicates(constitutional_text__judicial_supremacy_reading, stare_decisis_settlement_value).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Life-tenured judges sitting as the apex court. They decide which laws stand, own the docket, and issue interpretations that bind every other public body; compliance runs through their contempt power and control of jurisdiction. Each settled controversy adds to the institution's store of doctrinal authority. Members exit only by death, retirement, or impeachment; the role and the institution's self-conception are the same thing.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, constitutional_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).

% Discrete groups that reliably lose ordinary political competition — unpopular sects, criminal defendants, marginalized ethnic and religious minorities. Protection arrives by lawsuit: they need counsel, standing, and a doctrinal hook. Their alternatives are emigration, concealment, or waiting for majoritarian sentiment to turn; none is timely.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, rights_claimant_minorities, beneficiary,
    powerless, biographical, trapped, national).

% Elected chambers and the coalitions that control them. Statutes they pass can be voided by court order after the fact, and no ordinary legislative act can reverse the ruling. Their available responses — supermajority amendment, appointment timing, proposals to limit jurisdiction — are slow, uncertain, and politically expensive; meanwhile their electoral mandate expires on a fixed clock.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, legislative_majorities, payer,
    powerful, immediate, constrained, national).

% The voting public. They receive stable rights guarantees and settled law like everyone else, but watch enacted preferences struck down by officials they cannot vote out. Their corrective tools — elections, amendment, appointment pressure — operate on cycles far longer than the litigation that produced the ruling.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, majoritarian_voters, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text__judicial_supremacy_reading, majoritarian_voters, beneficiary).

% Trial and intermediate appellate judges. They receive ready-made constitutional answers from above, which spares them ultimate responsibility for contested questions; in exchange they may not depart from controlling precedent, and their innovations are reversible at will.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, lower_courts, beneficiary,
    institutional, biographical, constrained, national).

% Movements and theorists who want constitutional questions resolved by referendum, citizens' assembly, rotating convention, or departmentalist executive interpretation. None of those forums exists inside the operating arrangement; they organize, publish, and campaign for structural change from outside a process that never pauses for them.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, direct_democracy_advocates, excluded,
    organized, generational, trapped, national).

% Academic lawyers and political scientists who compare how different systems allocate final interpretive authority — override clauses, advisory opinions, popular amendment. They testify, advise reform commissions, and publish critiques; they decide nothing and hold no docket.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, comparative_constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text__judicial_supremacy_reading, constitutional_judiciary).
narrative_ontology:fixing_cost_class(constitutional_text__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Settles disputes over constitutional meaning with a single authoritative interpreter, giving legislators, officials, and citizens a determinate answer to what the constitution permits; terminates inter-branch and federal-state conflicts that would otherwise recur indefinitely.
% TRANSFER_FUNCTION: Moves final decision-making authority over constitutional questions from elected legislatures (and behind them the electorate) to the judiciary; each invalidation transfers a live policy decision from the majority's representatives to unelected judges, irreversibly short of amendment.
% ABSENT_VOICES: Direct-democracy advocates and popular-constitutionalists who would resolve constitutional questions by referendum, convention, or departmentalist executive interpretation are structurally absent — the conversation occurs in courtrooms they cannot convene. Future generations bound by precedents they never consented to are also unrepresented; both seats exist outside the process the arrangement administers.
% DISAPPEARANCE_RATIONALE: If judicial finality vanished overnight, every contested constitutional question would reopen among coordinate branches: legislatures would re-enact struck-down statutes, executives would assert independent interpretations, and rights protection would shift from litigation to political mobilization. Federal-state divisions, administrative review, and individual-rights guarantees would all be renegotiated through new mechanisms — the constitutional order would reorganize around whatever termination device replaced the court.
% FOUNDING_PROBLEM: In the early republic, coordinate branches and states each claimed authoritative constitutional interpretation, and disputes did not terminate — departmentalist claims, state defiance of federal law, and dueling branch interpretations produced recurring crises. The arrangement was built so that one institution's determination ends the dispute.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: political-science and comparative-law literatures document termination failures in systems lacking final interpreters; legislative-majority witnesses and executive-branch statements concede the settlement value while disputing judicial custody of it. The payer seats themselves attest the founding problem is real — no beneficiary-only attestation stands behind the genealogy.
narrative_ontology:disappearance_verdict(constitutional_text__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text__judicial_supremacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__judicial_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_text__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text__judicial_supremacy_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_text__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_text__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The arrangement carries a real coordination function — terminating interpretive crises that otherwise recur among coordinate branches — and a real transfer: each invalidation moves a policy decision from elected bodies to the bench, permanently, because override is unavailable. Extraction is therefore substantial but not total (epsilon 0.66 at interval end): the settlement value is genuine and survives hostile audit, which is what separates this profile from a pure-extraction arrangement. Suppression (0.70) is a raw structural property, unscaled by power or scope: finality is maintained by jurisdiction control, contempt, and stare decisis rather than by participant preference, and the legal profession's internalized equation of court-finality with 'rule of law' lowers the enforcement burden the structure would otherwise face. Theater (0.38 and trending up) tracks the growing share of institutional energy spent defending the court's own authority — legitimacy rhetoric, ceremonial neutrality claims, emergency-docket management — rather than deciding controversies. Accessibility_collapse (0.48) is moderate because the alternative designs are not hypothetical: parliamentary sovereignty, notwithstanding clauses, and popular-amendment regimes operate visibly in peer systems, so the alternative does not vanish on inspection; it is merely unreachable inside this order except at amendment-grade cost. Resistance (0.62) is episodic and coalition-driven: court-curbing proposals, jurisdiction-stripping bills, confirmation conflicts, and amendments reversing particular rulings surge after salient decisions, then subside — the payer seats' latent coalition (amendment plus appointment politics plus jurisdiction proposals) is what resistance actually consists of. The measurement series run on one shared ten-point grid (1803-2026) and show two full assertion-retreat cycles: an aggressive-review peak in the Lochner era collapsing after the 1937 confrontation, and a long Warren-to-Obergefell ascent partially retreating after 2022. The oscillation is driven by electoral realignments, appointment timing, and legitimacy crises; it is partly functional (each assertion cycle re-prices the cost of defiance) and partly an extraction mechanism in the intermittent-reinforcement sense — episodic assertion keeps both political branches uncertain which enactments will survive, discouraging the sustained coalition-building a stable rule would provoke. End-state values match the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the constitutional_judiciary seat the arrangement is indistinguishable from constitutionalism itself — finality is what 'having a constitution' means, and the seat is identity-locked to it (institutional identity fusion: the organization has become its guardianship function; were that frame to break and the court concede co-equal interpretive authority, the computed profile would soften toward transitional-support territory). From the legislative_majorities seat the same structure is a one-way ratchet: their enactments are voidable, the court's acts are not, and their exit instruments are amendment-grade. rights_claimant_minorities and lower_courts experience the structure as subsidy — protection and settled doctrine respectively — and compute near the beneficiary pole. majoritarian_voters straddle: diffuse benefit (stable rights, settled law) against diffuse cost (filtered preferences), which is why an override pins their directionality mid-range rather than letting the victim declaration push them to the target pole. Same-level divergence: legislative_majorities (powerful) and the constitutional_judiciary (institutional) hold comparable global standing yet opposite directionalities, differentiated entirely by constraint-specific factors — who owns finality and what exit costs look like from each chair.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows. constitutional_judiciary: declared beneficiary and agenda-setter, identity_locked exit — derived d sits near the beneficiary pole; the arrangement subsidizes it with decision authority. rights_claimant_minorities: beneficiary, powerless, trapped — d near zero; on this reading's own account the arrangement exists to subsidize exactly this seat. lower_courts: beneficiary, institutional, constrained — low d; receives settled doctrine. legislative_majorities: victim, powerful, constrained — high d; the transfer's direct payers, with exit priced at amendment grade. majoritarian_voters: declared victim with secondary beneficiary position, moderate power, constrained exit — the structural derivation would read the victim declaration alone and push d toward the target pole; the override to 0.55 encodes the dual position (diffuse benefit received, diffuse cost borne). direct_democracy_advocates: excluded, organized — no beneficiary/victim declaration exists for them, so the canonical fallback would misplace them; the override to 0.70 records that the arrangement forecloses their preferred decision forums entirely, placing them near the target pole. comparative_constitutional_scholars: analytical seat — outside the chi arithmetic. Scope note: the arrangement operates at national scope within each adopting polity; compliance verification is comparatively easy at that scale, so the scope amplification of effective extraction is modest. Receipt surface: the gains demonstrably accrue to the constitutional_judiciary seat — foreclosed legislative authority converts into docket control, doctrinal ownership, and institutional prestige; rights-claimants receive protection (benefit) but the extracted authority itself lands on the bench. Fixing cost is prohibitive: displacement requires amendment-grade consensus against an identity-locked incumbent and a benefiting bar.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy analysis guards both error directions. Reading the arrangement as pure coordination (rope) would erase the asymmetric transfer — the foreclosure of legislative finality is not overhead, it is the point of contention, and the victim declarations exist to force the engine to price it. Reading it as pure extraction (snare) would erase the settlement function, which survives hostile audit: systems without final interpreters exhibit recurring inter-branch deadlock, and even the arrangement's payers concede the termination value in testimony. The founding problem — terminating interpretive crises — is live, so no mandate obsolescence is declared; the drift path to watch is theater_ratio growth: if legitimacy maintenance crowds out adjudication further while settlement value decays, the profile migrates toward inertial performance, and the R5 mismatch consumer would flag dead-problem persistence. Currently status=live and verdict=world_rearranges agree, so no zombie flag is warranted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_allocation_omega,
    'This constraint is the judicial_supremacy_reading of kernel constitutional_text; the sibling readings (legislative_sovereignty_reading, popular_sovereignty_reading) instantiate distinct constraints with different victim sets. Which allocation of final interpretive authority does the constitutional text actually license, and where exactly does the inter-reading disagreement live?',
    'Comparative constitutional-design outcomes (override-clause adoption and usage, amendment frequency, court-curbing episodes) combined with original-public-meaning analysis of the text''s allocation provisions; convergence of design evidence and textual evidence would resolve the reading contest.',
    'Under the legislative sibling the victim set flips — rights-claimants lose their subsidized seat and democratic responsiveness is restored; under the popular sibling both courts and legislatures become agents of a principal retaining revocable authority, and this story''s epsilon would be reassessed upward as usurpation rather than stewardship.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_allocation_omega, conceptual, 'Which reading of the constitutional-text kernel governs, and where the readings structurally diverge (custody and conclusiveness of final interpretive authority).').

omega_variable(
    hybrid_override_possibility,
    'Is the impossibility of legislative override intrinsic to any final-authority arrangement, or a design contingency — can judicial ruling and legislative override coexist (notwithstanding-clause models) without collapsing rights protection?',
    'Compare rights outcomes and democratic-satisfaction measures in hybrid override systems against pure judicial-supremacy and pure parliamentary systems over matched periods.',
    'If hybrids protect rights comparably, part of the measured extraction is attributable to design choice rather than structural necessity, tightening this reading toward a lower-extraction coordination profile; if hybrids degrade rights protection, the foreclosure premise hardens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_override_possibility, empirical, 'Whether judicial supremacy and legislative override are structurally incompatible or merely alternative designs.').

omega_variable(
    diffuse_support_enforcement_basis,
    'Does enforcement of judicial finality rest on legal doctrine alone or on diffuse public acquiescence — and how much of the measured suppression is internalized in the legal profession''s equation of court-finality with rule of law?',
    'Legitimacy survey series, compliance behavior during legitimacy crises, and bar-elite attitude tracking across decision-salience cycles.',
    'If acquiescence-based, suppression_requirement understates fragility — a legitimacy collapse would raise enforcement costs discontinuously and could date a type transition; if doctrine-based, the current scalar is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diffuse_support_enforcement_basis, empirical, 'Doctrine versus acquiescence as the enforcement basis for finality.').

omega_variable(
    amendment_exit_real_or_nominal,
    'Is the constitutional-amendment route a usable exit for majoritarian_voters and legislative_majorities, or nominal — does its practical availability match its formal existence?',
    'Historical base rates: amendments proposed versus adopted, with attention to rights-reversal amendments specifically and to the appointment-politics substitute path.',
    'If nominal, both payer seats are effectively trapped and effective extraction rises above the authored base; if real, directionality stays mid-range and the constrained-exit atoms are honest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(amendment_exit_real_or_nominal, empirical, 'Practical availability of the amendment exit for the payer seats.').

omega_variable(
    cs_framing_underdetermination,
    'The declared framing locates the kernel in the constitutional text wielded by the court as institution; an alternative framing locates it in the finality doctrine itself — the legitimacy claim (''invalidation is conclusive'') layered above the institution. Do the two framings classify alike?',
    'Re-classify under the doctrine-as-kernel framing: authority_grounding shifts toward extraction (the court''s authority feeds on preventing kernel revision), and the interpretation layer becomes the doctrine''s own glossators rather than the bench.',
    'Under the alternative framing the commitment-system pattern changes (extraction-grounded rather than lineage-grounded authority), altering contamination-network predictions and the drift terminal state; signals favoring the declared framing: the reading''s own text locates the grant in the written constitution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Text-and-institution versus finality-doctrine framings of the kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__judicial_supremacy_reading, 1803, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t1803, constitutional_text__judicial_supremacy_reading, theater_ratio, 1803, 0.12).
narrative_ontology:measurement(cons_tr_t1857, constitutional_text__judicial_supremacy_reading, theater_ratio, 1857, 0.18).
narrative_ontology:measurement(cons_tr_t1905, constitutional_text__judicial_supremacy_reading, theater_ratio, 1905, 0.31).
narrative_ontology:measurement(cons_tr_t1937, constitutional_text__judicial_supremacy_reading, theater_ratio, 1937, 0.22).
narrative_ontology:measurement(cons_tr_t1954, constitutional_text__judicial_supremacy_reading, theater_ratio, 1954, 0.27).
narrative_ontology:measurement(cons_tr_t1973, constitutional_text__judicial_supremacy_reading, theater_ratio, 1973, 0.33).
narrative_ontology:measurement(cons_tr_t2000, constitutional_text__judicial_supremacy_reading, theater_ratio, 2000, 0.36).
narrative_ontology:measurement(cons_tr_t2015, constitutional_text__judicial_supremacy_reading, theater_ratio, 2015, 0.39).
narrative_ontology:measurement(cons_tr_t2022, constitutional_text__judicial_supremacy_reading, theater_ratio, 2022, 0.34).
narrative_ontology:measurement(cons_tr_t2026, constitutional_text__judicial_supremacy_reading, theater_ratio, 2026, 0.38).

% Extraction over time
narrative_ontology:measurement(cons_be_t1803, constitutional_text__judicial_supremacy_reading, base_extractiveness, 1803, 0.34).
narrative_ontology:measurement(cons_be_t1857, constitutional_text__judicial_supremacy_reading, base_extractiveness, 1857, 0.56).
narrative_ontology:measurement(cons_be_t1905, constitutional_text__judicial_supremacy_reading, base_extractiveness, 1905, 0.71).
narrative_ontology:measurement(cons_be_t1937, constitutional_text__judicial_supremacy_reading, base_extractiveness, 1937, 0.41).
narrative_ontology:measurement(cons_be_t1954, constitutional_text__judicial_supremacy_reading, base_extractiveness, 1954, 0.63).
narrative_ontology:measurement(cons_be_t1973, constitutional_text__judicial_supremacy_reading, base_extractiveness, 1973, 0.72).
narrative_ontology:measurement(cons_be_t2000, constitutional_text__judicial_supremacy_reading, base_extractiveness, 2000, 0.74).
narrative_ontology:measurement(cons_be_t2015, constitutional_text__judicial_supremacy_reading, base_extractiveness, 2015, 0.77).
narrative_ontology:measurement(cons_be_t2022, constitutional_text__judicial_supremacy_reading, base_extractiveness, 2022, 0.64).
narrative_ontology:measurement(cons_be_t2026, constitutional_text__judicial_supremacy_reading, base_extractiveness, 2026, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t1803, constitutional_text__judicial_supremacy_reading, suppression_requirement, 1803, 0.28).
narrative_ontology:measurement(cons_su_t1857, constitutional_text__judicial_supremacy_reading, suppression_requirement, 1857, 0.52).
narrative_ontology:measurement(cons_su_t1905, constitutional_text__judicial_supremacy_reading, suppression_requirement, 1905, 0.61).
narrative_ontology:measurement(cons_su_t1937, constitutional_text__judicial_supremacy_reading, suppression_requirement, 1937, 0.34).
narrative_ontology:measurement(cons_su_t1954, constitutional_text__judicial_supremacy_reading, suppression_requirement, 1954, 0.57).
narrative_ontology:measurement(cons_su_t1973, constitutional_text__judicial_supremacy_reading, suppression_requirement, 1973, 0.66).
narrative_ontology:measurement(cons_su_t2000, constitutional_text__judicial_supremacy_reading, suppression_requirement, 2000, 0.69).
narrative_ontology:measurement(cons_su_t2015, constitutional_text__judicial_supremacy_reading, suppression_requirement, 2015, 0.73).
narrative_ontology:measurement(cons_su_t2022, constitutional_text__judicial_supremacy_reading, suppression_requirement, 2022, 0.67).
narrative_ontology:measurement(cons_su_t2026, constitutional_text__judicial_supremacy_reading, suppression_requirement, 2026, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_text__judicial_supremacy_reading, constitutional_text__legislative_sovereignty_reading).
narrative_ontology:affects_constraint(constitutional_text__judicial_supremacy_reading, constitutional_text__popular_sovereignty_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'who decides what the constitution means' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints sharing the kernel constitutional_text. Each reading has its own epsilon, beneficiary/victim sets, and classification: this reading's victim set centers on legislative authority and democratic responsiveness; the legislative sibling's victim set centers on rights-claimants exposed to override; the popular sibling's on both courts and legislatures as agents of the demos. This file links to both siblings. Upstream/downstream: this reading influences its siblings' operating environments — judicial finality raises the stakes and cost of the amendment channels the popular reading depends on, and supplies the adversary that override-clause designs define themselves against.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_text__judicial_supremacy_reading, moderate, 0.55).
constraint_indexing:directionality_override(constitutional_text__judicial_supremacy_reading, organized, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
