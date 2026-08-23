% ============================================================================
% CONSTRAINT STORY: constitutional_interpretive_authority__parliamentary_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_interpretive_authority__parliamentary_supremacy_reading, []).

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
 *   constraint_id: constitutional_interpretive_authority__parliamentary_supremacy_reading
 *   human_readable: Parliamentary Supremacy Reading of Constitutional Interpretive Authority
 *   domain: legal/political
 *
 * SUMMARY:
 *   This story instantiates ONE reading — the parliamentary supremacy reading
 *   — of the contested kernel constitutional_interpretive_authority: the
 *   commitment that a constitution exists and demands a final interpreter.
 *   Under this reading the elected legislature holds final interpretive
 *   authority, no court may void its acts, and coercion is legitimated by
 *   electoral mandate rather than by rights-grounded judicial guardianship.
 *   Per the epsilon-invariance principle, the sibling readings (judicial
 *   supremacy; coordinate construction) are separate constraints in separate
 *   files with their own epsilon values and beneficiary/victim structures;
 *   this file authors only the parliamentary reading, whose epsilon referent
 *   is the standing parliamentary-supremacy arrangement itself, assessed by
 *   this reading's own lights. KEY AGENTS (by structural relationship): -
 *   elected_legislature_as_institution: agenda-setter
 *   (institutional/arbitrage) — administers the settlement and defines its
 *   own limits; - governing_parliamentary_majority: primary beneficiary
 *   (powerful/arbitrage) — collects interpretive discretion each electoral
 *   cycle; - electorate_voters: coordination beneficiary with secondary payer
 *   exposure (organized/mobile) — authorizes and dismisses the final
 *   interpreter; - legislative_minorities: payer (organized/constrained) —
 *   outvoted, no institutional check; - rights_claimant_minorities: primary
 *   target (powerless/trapped) — bear unreviewable acts indefinitely; -
 *   constitutional_judiciary: payer (institutional/identity_locked) — denied
 *   the final word its professional tradition aspires to; -
 *   disenfranchised_residents: excluded (powerless/trapped) — governed
 *   without electoral or judicial recourse; -
 *   comparative_constitutional_scholars: analytical observer — sees the full
 *   structure and its rivals.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.52).
domain_priors:suppression_score(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.58).
domain_priors:theater_ratio(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.27).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 0.27).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_interpretive_authority__parliamentary_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_interpretive_authority__parliamentary_supremacy_reading, "Parliamentary Supremacy Reading of Constitutional Interpretive Authority").
narrative_ontology:topic_domain(constitutional_interpretive_authority__parliamentary_supremacy_reading, "legal/political").

domain_priors:requires_active_enforcement(constitutional_interpretive_authority__parliamentary_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_interpretive_authority__parliamentary_supremacy_reading, '232fe4b1-2f29-4652-8389-1d5e7b635e46').
narrative_ontology:cs_kernel_codification('232fe4b1-2f29-4652-8389-1d5e7b635e46', distributed).
narrative_ontology:cs_authority_grounding('232fe4b1-2f29-4652-8389-1d5e7b635e46', practice).
narrative_ontology:cs_interpretation_layer_present('232fe4b1-2f29-4652-8389-1d5e7b635e46').
narrative_ontology:cs_reading_relation('232fe4b1-2f29-4652-8389-1d5e7b635e46', constitutional_interpretive_authority__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('232fe4b1-2f29-4652-8389-1d5e7b635e46', constitutional_interpretive_authority__coordinate_construction_reading, forecloses).
narrative_ontology:cs_axiom('232fe4b1-2f29-4652-8389-1d5e7b635e46', foundational, electoral_mandate_confers_interpretive_finality).
narrative_ontology:cs_axiom_status(electoral_mandate_confers_interpretive_finality, holdable).
narrative_ontology:cs_axiom_grounding('232fe4b1-2f29-4652-8389-1d5e7b635e46', electoral_mandate_confers_interpretive_finality, conventional).
narrative_ontology:cs_axiom('232fe4b1-2f29-4652-8389-1d5e7b635e46', foundational, unelected_judicial_nullification_illegitimate).
narrative_ontology:cs_axiom_status(unelected_judicial_nullification_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('232fe4b1-2f29-4652-8389-1d5e7b635e46', unelected_judicial_nullification_illegitimate, instrumental).
narrative_ontology:cs_reference_frame('232fe4b1-2f29-4652-8389-1d5e7b635e46', crown_in_parliament_electoral_finality).
narrative_ontology:cs_drift_state('232fe4b1-2f29-4652-8389-1d5e7b635e46', contemporary_rights_review_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('232fe4b1-2f29-4652-8389-1d5e7b635e46', '2026-06-12T10:15:00Z').
narrative_ontology:cs_kernel_id(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__parliamentary_supremacy_reading, elected_legislature_as_institution).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__parliamentary_supremacy_reading, governing_parliamentary_majority).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__parliamentary_supremacy_reading, electorate_voters).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__parliamentary_supremacy_reading, legislative_minorities).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__parliamentary_supremacy_reading, rights_claimant_minorities).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_judiciary).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__parliamentary_supremacy_reading, electorate_voters).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__parliamentary_supremacy_reading, dicean_parliamentary_sovereignty).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__parliamentary_supremacy_reading, mandate_theory_of_representative_democracy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The legislature (Crown-in-Parliament in Westminster systems) makes, unmakes, and reinterprets the constitutional settlement through ordinary statute. It defines the limits of its own authority: no act it passes is voidable by any other body, and a later statute supersedes an earlier one. It grants and revokes the jurisdiction of courts and tribunals by statute. Its room for maneuver is maximal: it can restructure the settlement, entrench or disentrench rules, and redefine who interprets what, at any sitting.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, elected_legislature_as_institution, agenda_setter,
    institutional, generational, arbitrage, national).

% The party or coalition commanding a working majority converts electoral wins into unchecked legislative control for the life of a parliament: it drafts, passes, and authoritatively reads the scope of its own acts, faces no judicial veto, and answers only to the next election. What flows to it is discretionary power over constitutional meaning; what flows from it is the risk of electoral defeat. It can legislate exceptions for itself, but losing office puts it on the minority side of the same arrangement.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, governing_parliamentary_majority, beneficiary,
    powerful, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__parliamentary_supremacy_reading, governing_parliamentary_majority, agenda_setter).

% Voters collectively authorize the final interpreter and can dismiss it at elections, which is the arrangement's accountability mechanism. They receive responsive, decisive government and clear assignment of responsibility. They also bear the costs of majority overreach whenever they sit on the losing side of a vote, with recourse limited to persuading a future majority to reverse course.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, electorate_voters, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__parliamentary_supremacy_reading, electorate_voters, payer).

% Opposition parties and backbench dissenters vote against acts they regard as unconstitutional and watch them take effect regardless. Their tools are argument, amendment, delay, and the next election; they hold no institutional check on the majority's interpretation. Leaving the process entirely would mean ceding what little influence they have, so they remain inside a process they cannot control.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, legislative_minorities, payer,
    organized, biographical, constrained, national).

% Individuals and groups whose interests collide with majority policy — religious minorities, protesters, asylum seekers, unpopular defendants — bear the direct costs of acts no court can strike down. Litigation can clarify application but cannot invalidate; their protection depends wholly on the majority's self-restraint or its fear of electoral consequence. Persistent minorities that never swing an election carry these costs indefinitely, and emigration is rarely a realistic option.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, rights_claimant_minorities, payer,
    powerless, generational, trapped, national).

% Judges interpret statutes daily and resolve application disputes, but the settlement denies them the last word: an act finally means what the legislature says it means, and no judgment can void one. The senior bench's inherited aspiration to guard fundamental law is structurally refused; individual judges may resign, but the judiciary as an institution cannot opt out of the station assigned to it, and its professional self-understanding is bound up with the subordinate interpretive role the settlement preserves for it.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_judiciary, payer,
    institutional, generational, identity_locked, national).

% Non-citizen residents, prisoners, and others subject to legislation without a vote have neither electoral recourse nor judicial invalidation available. They live under the arrangement's outputs while holding no seat in authorizing or checking them; their objections reach the process only indirectly, through citizen allies, NGOs, or parliamentary committees.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, disenfranchised_residents, excluded,
    powerless, biographical, trapped, national).

% Academic jurists and political theorists track how the settlement performs against rival designs — entrenched bills of rights with judicial review, dialogic inter-branch models — and document drift, stress episodes, and outcomes for minorities. They hold no power over the arrangement and no stake in its continuation beyond analytic interest.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_interpretive_authority__parliamentary_supremacy_reading, governing_parliamentary_majority).
narrative_ontology:fixing_cost_class(constitutional_interpretive_authority__parliamentary_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes a terminal answerer for constitutional questions: every dispute about what the constitution permits terminates in a decision by an identifiable, electorally accountable institution, avoiding inter-branch stalemate and the regress of interpreting the interpreter.
% TRANSFER_FUNCTION: Moves interpretive discretion and policy initiative from courts and constitutional minorities to electoral majorities; moves the costs of unreviewable legislation onto outvoted minorities, rights claimants, and everyone subject to the acts who lacks a vote.
% ABSENT_VOICES: Disenfranchised residents subject to the acts, future generations bound by settlements they never authorized, and the judicial voice arguing for rights guardianship are present only as petitioners or commentators — none holds a veto or a seat in maintaining the settlement.
% DISAPPEARANCE_RATIONALE: If final legislative authority vanished overnight, constitutional questions would have no terminal answer: courts would either assume guardianship (moving to the judicial-supremacy arrangement) or authority would fragment into ongoing inter-branch negotiation (the coordinate arrangement). Party government, statutory interpretation practice, and minority-rights litigation strategies all presuppose the current allocation and would reorganize around whichever successor emerged.
% FOUNDING_PROBLEM: After the rejection of hereditary and divine right, the settlement needed a location for ultimate constitutional authority that did not recreate absolute monarchy: the answer built was that the elected representatives of the nation, assembled in parliament, hold whatever sovereignty the crown surrendered.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians outside the benefiting parties corroborate the anti-absolutist origin (the 1688-89 settlement's purpose of subordinating the crown), while senior judges, rights advocates, and comparative scholars attest that the original problem is solved and the arrangement now primarily serves majority convenience — public critiques from former senior judges and the academic literature on elective dictatorship are explicit external attestations that the founding rationale no longer describes the arrangement's operative function.
narrative_ontology:disappearance_verdict(constitutional_interpretive_authority__parliamentary_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_interpretive_authority__parliamentary_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_interpretive_authority__parliamentary_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_interpretive_authority__parliamentary_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_interpretive_authority__parliamentary_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_interpretive_authority__parliamentary_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_interpretive_authority__parliamentary_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.52 (interval end): the arrangement genuinely coordinates (terminal answerability, decisive government, clear accountability) and genuinely transfers (unreviewable acts impose uncompensated costs on outvoted and trapped minorities). The temporal series is U-shaped rather than monotonic: franchise expansion through 1911-1966 broadened the beneficiary set and damped per-capita extraction (0.62 down to 0.36), after which rights-intensive legislation — immigration, detention, protest regulation — concentrated costs back onto persistent minorities, lifting extraction to 0.52. Suppression (0.58) is a raw structural property, unscaled by power or scope: it reflects the active machinery needed to deny courts the voiding power — early active suppression of rival sovereign claims (0.72 in 1689), consolidation into convention (0.42 by 1966), then managed accommodation and renewed assertion as rights instruments and devolution settlements pressed on the settlement (back to 0.58). Theater stays low-to-moderate (0.27): the interpretive and legislative functions are real, but a growing share of activity — second-reading rituals, select-committee scrutiny, ministerial rights-compliance statements — is performative assurance rather than operative check. Accessibility collapse is moderate (0.45): within a committed jurisdiction the alternative collapses for insiders, but rival readings remain live across jurisdictions and in scholarly and judicial discourse, and several polities have switched. Resistance is substantial (0.55): litigation, rights campaigning, devolution bargaining, and periodic court-curbing controversies. All three metric series run on one shared time grid (1689, 1832, 1911, 1966, 1998, 2005, 2024) so every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/agenda-setter seats compute differently from the same structural data. The sharpest divergence is the same-level institutional pair: the legislature and the judiciary hold comparable nominal institutional power, yet the constraint allocates finality to one and strips it from the other — the legislature's exit is arbitrage (it can restructure the settlement at will) while the judiciary's exit is identity-locked (its professional self-conception is fused with the subordinate interpretive role the settlement assigns it). Coalition dynamics matter for the powerless seats: the arrangement's design channels resistance into electoral competition, converting what could be combined minority-coalition power into alternating-majority turnover — a safety valve that keeps measured resistance moderate even though trapped minority seats individually have no leverage. The engine computes per-seat classifications from the structural data; the authored claim does not adjudicate the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit near the subsidy end: the legislature as institution and the governing majority collect interpretive discretion directly (low d); the electorate sits mildly above the beneficiary pole because its secondary payer exposure (losing-side costs) offsets part of its coordination gain. Victims sit near the target end: rights-claimant minorities are trapped and identity-persistent, placing them nearest the full-target position; the judiciary's identity lock amplifies its target-side placement despite institutional power; legislative minorities are organized but structurally outvoted, giving them high d with somewhat better exit than the trapped seats. Disenfranchised residents are excluded rather than coordinated — their exclusion is part of what the electoral-mandate legitimation requires, since extending the franchise would dilute the mandate that justifies the arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — locating ultimate authority after the rejection of hereditary and divine right — has been substantively solved, but the arrangement's terminal-answerability function remains live: every constitutional order needs some resolution to the interpreter regress, and the contest is over where finality rests, not whether. Authoring the founding problem as contested (rather than dead) prevents the mismatch consumer from firing a zombie flag on a living function, while the documented transfer to minorities prevents the opposite error of reading the arrangement as pure coordination. The late-interval rise in extractiveness alongside stable coordination function is the signature the corpus watches for: rent-seeking layered onto a still-genuine coordination structure, which is why the claimed type is tangled_rope rather than rope or snare. If the founding problem were resolved by adopting a sibling reading, this constraint would not atrophy into a piton — it would be replaced wholesale, since its function is indivisible from its allocation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the kernel constitutional_interpretive_authority (reading: parliamentary_supremacy_reading). Which of the three readings — this one, judicial supremacy, or coordinate construction — stabilizes as the operative settlement, and what changes structurally if a sibling displaces it?',
    'Observe adoption and rejection events: jurisdictions adopting entrenched bills of rights with judicial nullification, court-curbing episodes, and inter-branch standoffs reveal which reading holds under stress.',
    'If the judicial supremacy reading displaces this one, the legislature exits the beneficiary set, the judiciary enters it, and the victim set shifts to legislative majorities whose acts become voidable; if coordinate construction stabilizes, no seat holds finality and extraction diffuses across branches.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: one of three mutually exclusive readings of the interpretive-authority kernel.').

omega_variable(
    dispersed_minority_accountability_gap,
    'Does electoral accountability actually discipline the arrangement''s costs for minorities that never command a majority anywhere, or does the accountability mechanism systematically fail for geographically dispersed persistent minorities?',
    'Compare rights-relevant legislative outcomes and reversal rates for geographically concentrated versus dispersed minorities under parliamentary supremacy arrangements.',
    'If dispersed minorities fare systematically worse, the effective burden on trapped minority seats exceeds this reading''s self-assessment, supporting a higher effective-extraction computation for those seats and drift toward the snare pole in rights-intensive domains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dispersed_minority_accountability_gap, empirical, 'Whether the electoral mandate mechanism reaches the arrangement''s worst-off targets.').

omega_variable(
    convention_vs_contingency_durability,
    'Is the absence of judicial nullification maintained by durable constitutional convention, or by contingent political conditions (party discipline, absence of existential crisis) that a severe stress episode could break?',
    'Examine stress episodes — wartime detention, emergency legislation, acts targeting unpopular groups — and whether courts, upper chambers, or electoral backlash restored limits afterward.',
    'If the settlement is contingent, its stability is overstated: under crisis the arrangement could harden (suppression spiking, drift toward snare) or dissolve toward the coordinate construction reading; if conventional, the current profile is robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(convention_vs_contingency_durability, empirical, 'Durability basis of the no-nullification settlement.').

omega_variable(
    weak_form_review_boundary,
    'Do declarations of incompatibility, devolution settlements, and referendum-entrenchment norms constitute partial judicial authority that already contradicts this reading''s core premise, or mere interpretive courtesy the legislature may disregard at will?',
    'Analyze whether any judicial declaration of incompatibility has ever effectively compelled legislative amendment against the governing majority''s will, and whether any devolution settlement has survived a determined parliamentary attempt to override it.',
    'If de facto judicial authority exists, the instantiated constraint is already a hybrid drifting toward the coordinate construction reading; the victim set widens to include the majority (whose acts become practically contestable) and epsilon for the judiciary seat falls below the full-target end.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(weak_form_review_boundary, conceptual, 'Boundary between weak-form judicial voice and substantive judicial authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_interpretive_authority__parliamentary_supremacy_reading, 1689, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t1689, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 1689, 0.12).
narrative_ontology:measurement_basis(cons_tr_t1689, observed).
narrative_ontology:measurement(cons_tr_t1832, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 1832, 0.15).
narrative_ontology:measurement_basis(cons_tr_t1832, observed).
narrative_ontology:measurement(cons_tr_t1911, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 1911, 0.18).
narrative_ontology:measurement_basis(cons_tr_t1911, observed).
narrative_ontology:measurement(cons_tr_t1966, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 1966, 0.2).
narrative_ontology:measurement_basis(cons_tr_t1966, observed).
narrative_ontology:measurement(cons_tr_t1998, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 1998, 0.22).
narrative_ontology:measurement_basis(cons_tr_t1998, observed).
narrative_ontology:measurement(cons_tr_t2005, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 2005, 0.24).
narrative_ontology:measurement_basis(cons_tr_t2005, observed).
narrative_ontology:measurement(cons_tr_t2024, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 2024, 0.27).
narrative_ontology:measurement_basis(cons_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(cons_be_t1689, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 1689, 0.62).
narrative_ontology:measurement_basis(cons_be_t1689, observed).
narrative_ontology:measurement(cons_be_t1832, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 1832, 0.58).
narrative_ontology:measurement_basis(cons_be_t1832, observed).
narrative_ontology:measurement(cons_be_t1911, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 1911, 0.48).
narrative_ontology:measurement_basis(cons_be_t1911, observed).
narrative_ontology:measurement(cons_be_t1966, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 1966, 0.36).
narrative_ontology:measurement_basis(cons_be_t1966, observed).
narrative_ontology:measurement(cons_be_t1998, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 1998, 0.44).
narrative_ontology:measurement_basis(cons_be_t1998, observed).
narrative_ontology:measurement(cons_be_t2005, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 2005, 0.47).
narrative_ontology:measurement_basis(cons_be_t2005, observed).
narrative_ontology:measurement(cons_be_t2024, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 2024, 0.52).
narrative_ontology:measurement_basis(cons_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t1689, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 1689, 0.72).
narrative_ontology:measurement_basis(cons_su_t1689, observed).
narrative_ontology:measurement(cons_su_t1832, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 1832, 0.66).
narrative_ontology:measurement_basis(cons_su_t1832, observed).
narrative_ontology:measurement(cons_su_t1911, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 1911, 0.55).
narrative_ontology:measurement_basis(cons_su_t1911, observed).
narrative_ontology:measurement(cons_su_t1966, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 1966, 0.42).
narrative_ontology:measurement_basis(cons_su_t1966, observed).
narrative_ontology:measurement(cons_su_t1998, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 1998, 0.5).
narrative_ontology:measurement_basis(cons_su_t1998, observed).
narrative_ontology:measurement(cons_su_t2005, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 2005, 0.53).
narrative_ontology:measurement_basis(cons_su_t2005, observed).
narrative_ontology:measurement(cons_su_t2024, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 2024, 0.58).
narrative_ontology:measurement_basis(cons_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_interpretive_authority__parliamentary_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_interpretive_authority__judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_interpretive_authority__coordinate_construction_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial debate over 'who guards the constitution' conflates three structurally distinct claims. This file authors only the parliamentary supremacy reading; the judicial supremacy reading (courts as rights guardians with nullification power) and the coordinate construction reading (no final authority; inter-branch dialogue) are separate stories with their own epsilon values, beneficiary/victim sets, and classifications. The readings differ in epsilon because each assesses the interpretive-authority arrangement by its own lights with a different seat structure: under this reading the legislature enters the beneficiary set for interpretive discretion and the judiciary is pushed toward the target end; under the judicial reading those positions invert. The parliamentary settlement is upstream historically — its persistence shapes the operating environment (legitimacy conditions, career incentives, litigation strategy) within which the sibling readings compete.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
