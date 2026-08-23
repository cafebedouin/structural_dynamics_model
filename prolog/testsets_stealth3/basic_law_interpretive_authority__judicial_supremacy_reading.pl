% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_authority__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_authority__judicial_supremacy_reading, []).

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
 *   constraint_id: basic_law_interpretive_authority__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy Reading: Courts as Terminal Constitutional Interpreters
 *   domain: constitutional law/political theory/institutional design
 *
 * SUMMARY:
 *   In consolidated constitutional democracies, courts hold final
 *   interpretive authority over constitutional meaning: their readings bind,
 *   legislation conflicting with them falls, and no other institution can
 *   overturn a ruling short of supermajority amendment. This story
 *   instantiates the judicial_supremacy_reading of the
 *   basic_law_interpretive_authority kernel, which grounds that arrangement
 *   in specialized legal expertise and insulation from political pressure.
 *   The arrangement has a real coordination function (single stable
 *   adjudication of contested meaning, rights protection for those without
 *   electoral strength) AND an asymmetric extraction structure (the judiciary
 *   accumulates institutional authority; legislatures and electoral
 *   majorities lose effective control of constitutional policy and bear
 *   gridlock and anticipatory-drafting costs). Hence the claimed type
 *   tangled_rope, with requires_active_enforcement true: judicial finality
 *   persists only because compliance is actively secured through review
 *   practice, appointment leverage, and professional norms. The epsilon
 *   referent is the standing judicial-supremacy arrangement itself, assessed
 *   by this reading's own lights -- the reading endorses the arrangement, so
 *   it rates the transfer of interpretive authority as substantially
 *   justified but does not deny that the transfer occurs; the metrics are
 *   authored as descriptive facts, independently of the claim. Interval
 *   indexing: t=0 approximates the mature postwar consolidation of judicial
 *   review (circa 1965 in reference democracies), t=60 the present; the grid
 *   is shared across all three tracked metrics.
 *
 * KEY AGENTS:
 *   - judiciary_institution: agenda-setter and primary beneficiary (institutional power / identity-locked exit) -- administers final interpretation, collects authority, prestige, and insulation
 *   - legal_profession: secondary beneficiary (organized / mobile) -- supplies the expertise the regime runs on, collects standing returns in fees, chairs, and clerkships
 *   - constitutional_minorities: protected beneficiary (powerless / trapped) -- receive rights shielding they cannot win at the ballot box
 *   - elected_legislatures: primary payer (powerful / constrained) -- lose enacted programs to review, bear anticipatory drafting and gridlock costs
 *   - electoral_majorities: payer (organized / constrained) -- preferences overridden between elections by unelected interpreters
 *   - executive_branch: dual-positioned beneficiary-payer (powerful / constrained) -- gains durable influence through appointments, pays when its own actions are struck down
 *   - popular_constitutionalism_advocates: excluded voice (moderate / constrained) -- denied any formal seat in adjudication by the expert-legal frame
 *   - constitutional_theorists: analytical observer (analytical / analytical) -- maps the structure across systems, collects nothing, pays nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_authority__judicial_supremacy_reading, 0.55).
domain_priors:suppression_score(basic_law_interpretive_authority__judicial_supremacy_reading, 0.4).
domain_priors:theater_ratio(basic_law_interpretive_authority__judicial_supremacy_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_authority__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_authority__judicial_supremacy_reading, "Judicial Supremacy Reading: Courts as Terminal Constitutional Interpreters").
narrative_ontology:topic_domain(basic_law_interpretive_authority__judicial_supremacy_reading, "constitutional law/political theory/institutional design").

domain_priors:requires_active_enforcement(basic_law_interpretive_authority__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_authority__judicial_supremacy_reading, '108bc1f7-51b7-4e2c-a300-64b945072034').
narrative_ontology:cs_kernel_codification('108bc1f7-51b7-4e2c-a300-64b945072034', fixed_text).
narrative_ontology:cs_authority_grounding('108bc1f7-51b7-4e2c-a300-64b945072034', expertise).
narrative_ontology:cs_interpretation_layer_present('108bc1f7-51b7-4e2c-a300-64b945072034').
narrative_ontology:cs_reading_relation('108bc1f7-51b7-4e2c-a300-64b945072034', basic_law_interpretive_authority__parliamentary_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('108bc1f7-51b7-4e2c-a300-64b945072034', basic_law_interpretive_authority__popular_constitutionalism_reading, forecloses).
narrative_ontology:cs_axiom('108bc1f7-51b7-4e2c-a300-64b945072034', foundational, courts_are_final_constitutional_interpreters).
narrative_ontology:cs_axiom_status(courts_are_final_constitutional_interpreters, holdable).
narrative_ontology:cs_axiom_grounding('108bc1f7-51b7-4e2c-a300-64b945072034', courts_are_final_constitutional_interpreters, conventional).
narrative_ontology:cs_axiom('108bc1f7-51b7-4e2c-a300-64b945072034', foundational, expertise_and_independence_warrant_interpretive_finality).
narrative_ontology:cs_axiom_status(expertise_and_independence_warrant_interpretive_finality, holdable).
narrative_ontology:cs_axiom_grounding('108bc1f7-51b7-4e2c-a300-64b945072034', expertise_and_independence_warrant_interpretive_finality, instrumental).
narrative_ontology:cs_reference_frame('108bc1f7-51b7-4e2c-a300-64b945072034', expert_insulated_final_interpretation).
narrative_ontology:cs_drift_state('108bc1f7-51b7-4e2c-a300-64b945072034', contemporary_backlash_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('108bc1f7-51b7-4e2c-a300-64b945072034', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_authority__judicial_supremacy_reading, basic_law_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__judicial_supremacy_reading, judiciary_institution).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__judicial_supremacy_reading, legal_profession).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__judicial_supremacy_reading, constitutional_minorities).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__judicial_supremacy_reading, elected_legislatures).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__judicial_supremacy_reading, electoral_majorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__judicial_supremacy_reading, executive_branch).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__judicial_supremacy_reading, executive_branch).
narrative_ontology:constraint_vindicates(basic_law_interpretive_authority__judicial_supremacy_reading, judicial_review_doctrine).
narrative_ontology:constraint_vindicates(basic_law_interpretive_authority__judicial_supremacy_reading, countermajoritarian_guardianship_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Courts staffed through career appointment pipelines decide which readings of the constitutional text bind, invalidate legislation that conflicts, and hold that position against all comers short of supermajority amendment. Authority, prestige, budgetary independence, and expanded dockets flow to the institution as long as finality holds. The institution cannot step outside the interpretive function without ceasing to be what it is: surrendering final interpretive authority would dissolve the judiciary's self-conception, its recruitment bargain, and its claim to equal constitutional standing with the elected branches.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, judiciary_institution, agenda_setter,
    institutional, generational, identity_locked, national).

% Judges, litigators, and legal academics supply the specialized expertise the regime runs on. Because interpretive authority makes legal training decisive over the highest-stakes political questions, the profession collects standing returns: demand for constitutional counsel, endowed chairs, clerkship pipelines, and gatekeeping power over who may credibly speak about the constitution. Individual members move freely into politics, business, or retirement, but the class as a whole prospers under the arrangement and staffs its administration.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, legal_profession, beneficiary,
    organized, biographical, mobile, national).

% Religious dissenters, ethnic and linguistic minorities, and unpopular political movements obtain protection from courts that they cannot win at the ballot box and could not win in a legislature controlled by their opponents. They cannot relocate away from the constitution that governs them and possess no alternative enforcement forum; their stake is entirely dependent on courts continuing to exercise review against majoritarian legislation.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, constitutional_minorities, beneficiary,
    powerless, biographical, trapped, national).

% Legislatures draft and enact statutes that courts may invalidate after the fact. Coalition bargaining must anticipate the judicial veto -- provisions are softened, dropped, or judicial-proofed before passage -- and flagship programs that survive enactment can still fall to a single ruling. The majority's mandate buys less than it appears to. Escape routes exist in principle (supermajority amendment, jurisdiction-stripping statutes, court-curbing bills) but each carries prohibitive coordination costs or regime-level risks, so the loss is absorbed term after term.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, elected_legislatures, payer,
    powerful, biographical, constrained, national).

% Voting coalitions discover between elections that policies they won on are unconstitutional. Their preferences are overridden by unelected interpreters, and their remedy runs through appointment timing they do not control: they can organize, vote, and lobby for sympathetic nominees, but they cannot directly reinterpret the text their representatives enacted. The override arrives without a corresponding electoral event they could punish.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, electoral_majorities, payer,
    organized, immediate, constrained, national).

% Executives shape the interpreter through appointments, converting temporary office into durable influence over constitutional meaning well beyond their own term -- a benefit unavailable under any rival reading. At the same time, executive orders, war powers, and administrative rules are frequent review targets, so the same branch pays when its own actions are struck down. It holds a lever on the constraint and a liability to it simultaneously.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, executive_branch, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__judicial_supremacy_reading, executive_branch, payer).

% Scholars, civic organizers, and some legislators hold that citizens and their representatives should treat constitutional meaning as theirs to contest and settle, with judicial rulings treated as advisory input rather than terminal answers. The expert-legal frame grants them no formal seat: lay interpretation carries no binding authority anywhere in the regime's own procedures, so their objection registers only as commentary, publication, and occasional political pressure -- never as adjudication.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, popular_constitutionalism_advocates, excluded,
    moderate, generational, constrained, national).

% Comparative scholars map how different systems allocate interpretive finality, tracking compliance rates, amendment frequencies, and legitimacy trajectories across jurisdictions and centuries. They collect no rents from the arrangement and bear none of its costs; their analyses feed reform debates in multiple countries and provide the outside attestation the genealogy interview requires.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, constitutional_theorists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_authority__judicial_supremacy_reading, judiciary_institution).
narrative_ontology:fixing_cost_class(basic_law_interpretive_authority__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, stable, expert adjudication of contested constitutional meaning: interbranch disputes, federal conflicts, and rights claims are resolved once through a common procedure instead of being relitigated as political crises in every session. Insulates constitutional commitments from transient factional capture and gives officials and citizens predictable rules.
% TRANSFER_FUNCTION: Moves effective control over constitutional policy outcomes -- and the authority, prestige, and agenda-setting power attached to it -- from elected legislatures and electoral majorities to unelected courts; moves decision costs from political processes (elections, bargaining, amendment) to legal processes (litigation, interpretation, compliance).
% ABSENT_VOICES: Popular constitutionalists and ordinary citizens outside the legal profession would object that terminal interpretive authority belongs to the demos or its representatives; they are structurally absent because the expert-legal frame recognizes no lay seat in adjudication. Legislators whose programs were struck down likewise have no formal venue in which their account of the constitution's meaning counts for anything.
% DISAPPEARANCE_RATIONALE: If courts lost final interpretive authority overnight, legislatures would reclaim interpretation and begin overriding rulings immediately, unresolved interbranch disputes would accumulate into recurring constitutional crises or ad hoc political settlements, rights currently shielded by review would depend entirely on electoral fortune, and the legal profession's constitutional role would contract sharply -- the entire architecture of appointment politics, judicial-proofed drafting, and litigation-based rights enforcement would reorganize around whichever alternative interpretive authority consolidated.
% FOUNDING_PROBLEM: Constitutional texts are abstract and persistently contestable; without a terminal interpreter, every serious political disagreement risks becoming an unresolvable constitutional crisis, and constitutional commitments are hostage to whichever faction temporarily controls the legislature. The arrangement was built to place interpretation with an institution insulated from electoral pressure and equipped with specialized legal judgment.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: comparative constitutional scholarship documents both the persistence of interpretive conflict and the historical record of systems that lacked terminal adjudication; legislators across jurisdictions who contest judicial supremacy nonetheless continue to seek judicial validation of their own constitutional claims, attesting that the underlying dispute-resolution problem is real even while disputing the seat assignment; political scientists aligned with the sibling readings concede the coordination problem exists and dispute only its solution. No attesting source is a beneficiary of the arrangement in the relevant sense except the judiciary itself, whose testimony is discounted accordingly.
narrative_ontology:disappearance_verdict(basic_law_interpretive_authority__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_authority__judicial_supremacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_authority__judicial_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(basic_law_interpretive_authority__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_authority__judicial_supremacy_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_authority__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_authority__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_authority__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction 0.55: the transfer of interpretive authority from elected institutions to courts is substantial and measurable (struck legislation, chilled policy agendas, appointment-dependent policy timelines) but bounded -- courts periodically defer, amendment remains a theoretical override, and part of what courts block they replace with enforced constitutional commitments. Suppression 0.40: enforcement runs through institutional compliance and legitimacy rather than raw coercion; alternatives (amendment, jurisdiction-stripping, defiance) exist but are individually prohibitive, so alternatives are narrowed rather than closed. Theater ratio 0.28: the adjudicative function is dominantly real, but a growing share of activity is performative maintenance -- opinion architecture staged as neutral expertise, ritualized deference exchanges between branches, symbolic confirmation hearings -- rising over the interval as legitimacy contests intensify. Accessibility collapse 0.60: within the legal frame, rival interpretive authorities collapse to non-binding status once judicial supremacy is understood, though the amendment route stays formally open. Resistance 0.55: recurring court-curbing bills, packing campaigns, open defiance episodes, and the scholarly popular-constitutionalism movement itself constitute persistent, organized resistance. Temporal story on one shared grid: base_extractiveness climbs monotonically (0.38 to 0.55) as review expands into ever more policy domains and the stakes of interpretation rise; theater_ratio drifts upward in parallel; suppression_requirement follows a rise-peak-decline arc (0.35 up to 0.50 at t=30, down to 0.40) -- enforcement capacity was deliberately built through the mid-interval consolidation era and has been eroding under backlash since, which is why suppression_requirement (not merely the static scalar) is tracked here: the story specifically traces enforcement-capacity change. The late-interval suppression decline against rising extraction is the signature the lifecycle detector should examine: extraction accumulating while enforcement weakens.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical structural facts. From the judiciary's agenda-setter seat the arrangement is guardianship it performs, not extraction it imposes -- expertise exercised under duty; the engine should compute a coordination-dominant view from that seat. From the legislature and electoral-majority seats the same structure operates as enforced dispossession: programs won at the ballot box die in court, and drafting must be done twice (once for the coalition, once for the anticipated veto). From the constitutional-minorities seat it is shelter -- the only institution that has ever overruled their tormentors. The executive seat straddles: appointment leverage makes it a partial owner of the interpreter and a frequent target of it. No authored claim adjudicates among these; the divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. judiciary_institution declares as beneficiary and agenda-setter with identity-locked exit: d sits near the beneficiary end (~0.05) -- the constraint subsidizes the institution it constitutes. legal_profession is a beneficiary with mobile, near-arbitrage-grade exit (individual members can leave; the class collects regardless), pushing d toward the extreme beneficiary end (~0.08). constitutional_minorities are beneficiaries with trapped exit: trapping amplifies extraction only for targets, so their d stays low (~0.18) -- they are subsidized, not squeezed. executive_branch mixes both flows (appointment gains, review losses), landing mid-range (~0.35). elected_legislatures and electoral_majorities are payers with constrained exit: d lands near the target end (~0.85 and ~0.90 respectively), and the engine scales their effective extraction upward accordingly. popular_constitutionalism_advocates hold no seat in the derivation -- exclusion is their structural fact, recorded as role, not as directionality. Suppression is authored as a raw structural property and is NOT scaled by power or scope; only extractiveness is scaled, by directionality and the national scope of the arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem -- constitutional disputes otherwise escalate into unresolvable interbranch crises -- remains live, corroborated by sources outside the benefiting parties, so this is not a mandatrophy case and the constraint is not maintained theatrically in its core function. The mandatrophy-relevant risk runs forward, not backward: the suppression_requirement series shows enforcement capacity eroding while theater_ratio and extraction climb. If compliance norms finish decaying before any alternative interpretive settlement consolidates, the arrangement drifts toward piton (adjudication performed for an audience that no longer obeys) or toward snare (finality enforced without the coordination payoff that justified it). Tracking the three series on one grid is what makes that fork detectable rather than asserted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_seat_of_finality,
    'This constraint is ONE reading (judicial_supremacy_reading) of the basic_law_interpretive_authority kernel: what structurally changes if a sibling reading is adopted instead?',
    'Adopting parliamentary_sovereignty_reading removes the judiciary from the beneficiary set and removes legislatures/electoral majorities from the victim set, relocating extraction onto whatever constrains legislative self-interpretation; adopting popular_constitutionalism_reading dissolves the agenda_setter seat entirely (no terminal interpreter exists to administer anything) and redistribuates both beneficiary and victim positions across contesting coalitions.',
    'The entire family classification flips with the reading choice: this story''s tangled_rope profile (coordination + judiciary-benefiting extraction) has no counterpart under popular constitutionalism, where the constraint-as-administered-arrangement ceases to exist. Cross-reading comparisons of epsilon are invalid; each sibling must be authored separately.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_seat_of_finality, conceptual, 'Committer-frame indexicality: epsilon, beneficiaries, and victims are properties of this reading, not of the shared kernel.').

omega_variable(
    terminal_arbiter_necessity,
    'Does a constitutional order require SOME terminal interpreter of last resort (making the seat structurally necessary even if its occupant changed), or is terminal adjudication itself optional?',
    'Comparative institutional analysis of systems that historically operated without judicial supremacy (traditional Westminster practice, pre-review parliamentary regimes): did they suffer unresolvable constitutional crises, or did political constitutionalism absorb interpretive disputes?',
    'If terminal adjudication is necessary, part of the measured extraction is the irreducible price of the coordination function and the judiciary''s rents are partially compensated; if it is optional, judicial supremacy''s coordination claim shrinks and the extraction share rises toward snare territory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(terminal_arbiter_necessity, empirical, 'Whether the coordination function justifies the beneficiary structure or merely clothes it.').

omega_variable(
    minority_protection_vs_rent_share,
    'What fraction of the judiciary''s benefit from final interpretive authority is legitimate compensation for rights protection of powerless groups, versus rent collected from positional authority?',
    'Case-level coding of review decisions across jurisdictions and decades: proportion of strikes protecting discrete and insular minorities versus proportion reallocating policy toward judicial preference or expanding institutional prerogative.',
    'A high protection share supports the tangled_rope reading (genuine coordination subsidizing the powerless); a collapsing share would push the effective profile toward snare, with constitutional_minorities converted from beneficiaries to abandoned parties.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_protection_vs_rent_share, empirical, 'Decomposition of the judiciary''s benefit into service-compensation and rent components.').

omega_variable(
    compliance_norm_basis,
    'Does the constraint''s enforcement rest on internalized professional-compliance norms (which decay with legitimacy shocks) or on structural dependency (which persists independently of belief)?',
    'Track compliance rates and defiance episodes following legitimacy shocks (contested appointments, unpopular landmark rulings, court-curbing legislative campaigns); distinguish episodes where officials complied despite hostility from episodes where enforcement machinery itself was altered.',
    'Norm-based enforcement predicts continued decay of the suppression_requirement series (piton drift risk: adjudication maintained theatrically while obedience erodes); dependency-based enforcement predicts re-hardening (snare drift risk). Determines which lifecycle branch the late-interval suppression decline belongs to.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_norm_basis, empirical, 'Structural versus internalized basis of the enforcement that sustains judicial finality.').

omega_variable(
    gridlock_cost_attribution,
    'Are the gridlock and anticipatory-drafting costs borne by the legislative process caused by judicial supremacy specifically, or by the underlying constitutional constraints that ANY interpreter would enforce?',
    'Natural experiments where review was curtailed or overridden (notwithstanding-clause usage, jurisdiction-stripping episodes, periods of executive defiance): measure legislative throughput, policy ambition, and outcome quality before and after.',
    'If costs persist under alternative interpreters, they belong to the constitution rather than to this constraint and the victim-set burden attributed here is overstated; if they disappear, judicial supremacy is their specific cause and the legislative victim declaration is exact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gridlock_cost_attribution, empirical, 'Attribution of the legislative-process burden between this constraint and the constitutional substrate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_authority__judicial_supremacy_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(basi_tr_t10, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement(basi_tr_t20, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement(basi_tr_t30, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(basi_tr_t40, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement(basi_tr_t50, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 50, 0.26).
narrative_ontology:measurement(basi_tr_t60, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 60, 0.28).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(basi_be_t10, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(basi_be_t20, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 20, 0.46).
narrative_ontology:measurement(basi_be_t30, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 30, 0.49).
narrative_ontology:measurement(basi_be_t40, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 40, 0.52).
narrative_ontology:measurement(basi_be_t50, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 50, 0.54).
narrative_ontology:measurement(basi_be_t60, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 60, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t0, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(basi_su_t10, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(basi_su_t20, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement(basi_su_t30, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement(basi_su_t40, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 40, 0.47).
narrative_ontology:measurement(basi_su_t50, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 50, 0.43).
narrative_ontology:measurement(basi_su_t60, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 60, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_authority__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__judicial_supremacy_reading, parliamentary_sovereignty_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__judicial_supremacy_reading, popular_constitutionalism_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'who interprets the constitution.' The label conflates three structurally distinct arrangements with different epsilon values, beneficiary sets, and failure modes: judicial supremacy (this file -- courts as terminal interpreters; judiciary in beneficiary set, legislatures and electoral majorities in victim set), parliamentary sovereignty (finality with the elected legislature; judiciary and courts-dependent minorities reposition), and popular constitutionalism (no terminal seat; extraction redistributes onto whichever coalition claims interpretive authority). The upstream reading in most jurisdictions is judicial supremacy -- its operation creates the structural conditions (blocked legislation, appointment politics, legitimacy contests) that pressure the sibling readings' operating environments -- hence the affects_constraints edges run from this story to both siblings. Each family member must carry reciprocal edges; orphan readings are a code smell.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
