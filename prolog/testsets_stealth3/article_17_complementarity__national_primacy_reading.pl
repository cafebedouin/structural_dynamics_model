% ============================================================================
% CONSTRAINT STORY: article_17_complementarity__national_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_17_complementarity__national_primacy_reading, []).

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
 *   constraint_id: article_17_complementarity__national_primacy_reading
 *   human_readable: Article 17 Complementarity — National Primacy Reading (Sovereignty-Protection Instantiation)
 *   domain: international_law/criminal_justice/state_sovereignty
 *
 * SUMMARY:
 *   Article 17 of the Rome Statute allocates jurisdiction between national
 *   systems and the International Criminal Court through the complementarity
 *   principle. This story instantiates one reading of that kernel — the
 *   national primacy reading — under which national courts are presumptively
 *   adequate unless demonstrated to be shams, the Court bears the affirmative
 *   burden of proving inadmissibility, and the effective victim set shrinks
 *   to cases of complete institutional collapse. KEY AGENTS (by structural
 *   relationship): see commentary.key_agents; the same agents populate
 *   beneficiaries/victims and the structured stakeholder surface. The
 *   claim/metric relationship is deliberately unreconciled: claimed_type is
 *   authored from what is structurally true of this reading-instantiated
 *   arrangement — a real coordination core wrapped around a restrictive
 *   victim-set transfer — while the metrics describe its observed operation.
 *   The ε referent is the standing high-threshold arrangement itself, never
 *   the sibling oversight reading's endorsed alternative; the sibling is a
 *   different constraint, linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - - icc_pretrial_chambers: Case-level agenda_setter (institutional/constrained) — renders admissibility determinations under an affirmative proof burden
 *   - - assembly_of_states_parties: Rule-level agenda_setter with beneficiary position (institutional/constrained) — codifies, funds, and politically steers the allocation rule
 *   - - national_judiciaries: Primary beneficiary (institutional/constrained) — collect jurisdictional primacy; their proceedings gatekeep international reach
 *   - - sovereignty_maximizing_states: Primary beneficiary (powerful/arbitrage) — champion the narrow reading, monetize cooperation, withdraw when crossed
 *   - - incumbent_executive_networks: Concentrated beneficiary (powerful/arbitrage) — capture the impunity space that deference to national process creates
 *   - - atrocity_victims_weak_genuine_states: Primary payer (powerless/trapped) — cases ruled inadmissible behind selectively genuine proceedings
 *   - - defendants_biased_national_trials: Secondary payer (powerless/trapped) — lose independent adjudication once a biased process clears the genuineness bar
 *   - - collapsed_state_victim_communities: Protected beneficiary (powerless/trapped) — retain international access because total collapse defeats even the narrow incapacity test
 *   - - target_state_civil_society: Excluded voice (organized/constrained) — holds the counter-evidence, lacks standing in challenge proceedings
 *   - - human_rights_litigation_ngos: Analytical observer (organized/analytical) — amicus advocacy and comparative case analysis
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_17_complementarity__national_primacy_reading, 0.62).
domain_priors:suppression_score(article_17_complementarity__national_primacy_reading, 0.55).
domain_priors:theater_ratio(article_17_complementarity__national_primacy_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_17_complementarity__national_primacy_reading, tangled_rope).
narrative_ontology:human_readable(article_17_complementarity__national_primacy_reading, "Article 17 Complementarity — National Primacy Reading (Sovereignty-Protection Instantiation)").
narrative_ontology:topic_domain(article_17_complementarity__national_primacy_reading, "international_law/criminal_justice/state_sovereignty").

domain_priors:requires_active_enforcement(article_17_complementarity__national_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_17_complementarity__national_primacy_reading, '6255cc1a-36b9-4df7-b356-3c6df9d8d053').
narrative_ontology:cs_kernel_codification('6255cc1a-36b9-4df7-b356-3c6df9d8d053', fixed_text).
narrative_ontology:cs_authority_grounding('6255cc1a-36b9-4df7-b356-3c6df9d8d053', lineage).
narrative_ontology:cs_interpretation_layer_present('6255cc1a-36b9-4df7-b356-3c6df9d8d053').
narrative_ontology:cs_reading_relation('6255cc1a-36b9-4df7-b356-3c6df9d8d053', article_17_complementarity__international_oversight_reading, coexists_with).
narrative_ontology:cs_axiom('6255cc1a-36b9-4df7-b356-3c6df9d8d053', foundational, sovereign_consent_bounds_court_authority).
narrative_ontology:cs_axiom_status(sovereign_consent_bounds_court_authority, holdable).
narrative_ontology:cs_axiom_grounding('6255cc1a-36b9-4df7-b356-3c6df9d8d053', sovereign_consent_bounds_court_authority, deontological).
narrative_ontology:cs_axiom('6255cc1a-36b9-4df7-b356-3c6df9d8d053', foundational, national_courts_presumptively_adequate).
narrative_ontology:cs_axiom_status(national_courts_presumptively_adequate, holdable).
narrative_ontology:cs_axiom_grounding('6255cc1a-36b9-4df7-b356-3c6df9d8d053', national_courts_presumptively_adequate, conventional).
narrative_ontology:cs_axiom('6255cc1a-36b9-4df7-b356-3c6df9d8d053', secondary, icc_bears_inadmissibility_burden).
narrative_ontology:cs_axiom_status(icc_bears_inadmissibility_burden, holdable).
narrative_ontology:cs_axiom_grounding('6255cc1a-36b9-4df7-b356-3c6df9d8d053', icc_bears_inadmissibility_burden, conventional).
narrative_ontology:cs_reference_frame('6255cc1a-36b9-4df7-b356-3c6df9d8d053', national_jurisdiction_primacy_frame).
narrative_ontology:cs_drift_state('6255cc1a-36b9-4df7-b356-3c6df9d8d053', contemporary_admissibility_practice, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('6255cc1a-36b9-4df7-b356-3c6df9d8d053', '').
narrative_ontology:cs_kernel_id(article_17_complementarity__national_primacy_reading, article_17_complementarity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_17_complementarity__national_primacy_reading, national_judiciaries).
narrative_ontology:constraint_beneficiary(article_17_complementarity__national_primacy_reading, sovereignty_maximizing_states).
narrative_ontology:constraint_beneficiary(article_17_complementarity__national_primacy_reading, incumbent_executive_networks).
narrative_ontology:constraint_beneficiary(article_17_complementarity__national_primacy_reading, collapsed_state_victim_communities).
narrative_ontology:constraint_victim(article_17_complementarity__national_primacy_reading, atrocity_victims_weak_genuine_states).
narrative_ontology:constraint_victim(article_17_complementarity__national_primacy_reading, defendants_biased_national_trials).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_17_complementarity__national_primacy_reading, assembly_of_states_parties).
narrative_ontology:constraint_vindicates(article_17_complementarity__national_primacy_reading, subsidiarity_doctrine).
narrative_ontology:constraint_vindicates(article_17_complementarity__national_primacy_reading, sovereign_equality_principle).
narrative_ontology:constraint_vindicates(article_17_complementarity__national_primacy_reading, positive_complementarity_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Determines which situations and cases proceed when a state with operating courts claims jurisdiction over the same conduct. Bears the affirmative burden of demonstrating that a national proceeding falls below minimum standards of independence and good faith before taking a case away from the state. Its determinations are formally challengeable, and each one settles whether particular victims ever reach this court.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, icc_pretrial_chambers, agenda_setter,
    institutional, generational, constrained, global).

% The treaty body of states joined to the Rome Statute. Codifies and amends the jurisdiction-allocation rule, sets the court's budget, and is the arena where sovereignty-minded blocs press deferential interpretations and negotiate cooperation politics. Its members collectively gain preserved domestic primacy while individually funding the institution that respects it.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, assembly_of_states_parties, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(article_17_complementarity__national_primacy_reading, assembly_of_states_parties, beneficiary).

% Domestic courts and prosecutors of states parties with functioning if uneven systems. Their proceedings form the primary track for atrocity crimes: whatever they credibly undertake removes the matter from international reach. They gain jurisdictional deference and capacity investment; in return they carry the expectation to prosecute credibly enough that the international court stays out.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, national_judiciaries, beneficiary,
    institutional, generational, constrained, national).

% States, often acting through regional blocs, that treat international criminal jurisdiction as an intrusion on sovereign equality. They champion the narrowest workable definition of a genuine national effort, reward aligned governments with cooperation and sanction critics by withholding arrests and access, and have withdrawn from the treaty outright when rulings cut against them. Running even minimal domestic proceedings reliably blocks international cases.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, sovereignty_maximizing_states, beneficiary,
    powerful, generational, arbitrage, global).

% Ruling political networks in affected states. Where they control the prosecutorial apparatus they can stage credible-looking trials of subordinates or opponents while insulating senior figures — and under a high bar for proving bad faith, that staging succeeds in keeping international investigators out. The impunity space created by deference to national process accrues here first.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, incumbent_executive_networks, beneficiary,
    powerful, biographical, arbitrage, national).

% Survivors of mass crimes in states that maintain functioning-but-selective courts. Because their government runs some credible prosecutions, their cases are ruled inadmissible internationally regardless of how much criminality the national process leaves unaddressed. They have no alternate international route: their access depends entirely on the same state apparatus that shields the perpetrators.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, atrocity_victims_weak_genuine_states, payer,
    powerless, biographical, trapped, national).

% Perpetrators or suspected perpetrators tried before politicized domestic courts that nonetheless clear the minimum-genuineness bar. They lose any prospect of independent international adjudication: a verdict rendered under visible bias stands as final because the process technically counted as genuine. Mid-level figures bear this most; senior shielded figures rarely reach trial at all.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, defendants_biased_national_trials, payer,
    powerless, biographical, trapped, national).

% Survivors in territories where state institutions have wholly collapsed and no functioning courts exist to claim the case. Complete institutional absence satisfies even the narrowest incapacity test, so the international court takes these situations. With the court's finite capacity defended from contested-jurisdiction fights, these communities remain the protected core of its caseload.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, collapsed_state_victim_communities, beneficiary,
    powerless, biographical, trapped, regional).

% Domestic human-rights organizations and victim advocates inside the states whose proceedings count. They document selective-prosecution patterns, command-chain omissions, and witness intimidation — precisely the evidence a stricter bad-faith test would require — but hold no formal standing in jurisdictional challenge proceedings, which run between states and the court.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, target_state_civil_society, excluded,
    organized, biographical, constrained, national).

% International advocacy and litigation organizations engaging through amicus submissions and comparative jurisprudence analysis. They argue for broader tests of governmental willingness and publish the case comparisons later decisions cite. They neither fund nor decide; their influence is argumentative.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, human_rights_litigation_ngos, observer,
    organized, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_17_complementarity__national_primacy_reading, incumbent_executive_networks).
narrative_ontology:fixing_cost_class(article_17_complementarity__national_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates prosecutorial jurisdiction between roughly two hundred national systems and one international court: it prevents duplicate prosecution of the same conduct, gives states a durable incentive to build domestic capacity, and secures the arrest, evidence, and access cooperation without which the international court cannot execute warrants at all.
% TRANSFER_FUNCTION: Moves adjudicative primacy to national judiciaries and the burden of proof onto the international court; concretely, it moves accountability access away from victims in states capable of mounting minimally credible proceedings and into processes those same states control, while concentrating the court's scarce capacity on collapsed-state cases.
% ABSENT_VOICES: Victim representatives and domestic civil society from weak-but-genuine states would contest nearly every deferential finding — they hold the documentation of selective prosecution — but admissibility contests formally run between challenging states and the court; affected populations appear as petitioners, not as parties with standing over the allocation rule itself.
% DISAPPEARANCE_RATIONALE: Without the primacy presumption and the burden rule, every atrocity situation in a functioning state becomes concurrently prosecutable; the court's docket would saturate immediately, states would confront an unbounded international claim on their jurisdictions, cooperation would harden or fracture, and the treaty system would either renegotiate its allocation terms or shed members — the entire architecture of who-may-prosecute rearranges.
% FOUNDING_PROBLEM: At the Rome conference, states would not join a permanent criminal court positioned above their own jurisdictions; negotiators needed a formula letting states keep first claim on their own atrocities while guaranteeing international backup where national systems were absent, complicit, or overwhelmed. Complementarity was that formula.
% FOUNDING_PROBLEM_CORROBORATION: Drafting-history scholarship and published preparatory-commission records attest that both purposes were present from the start: delegation statements from sovereignty-defending and accountability-first camps alike survive in the archive, and the earlier ICTY/ICTR primacy debates that framed Rome are documented independently of any current beneficiary's advocacy. No single camp owns the founding record — which is why the status reads contested rather than dead.
narrative_ontology:disappearance_verdict(article_17_complementarity__national_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_17_complementarity__national_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_17_complementarity__national_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_17_complementarity__national_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_17_complementarity__national_primacy_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_17_complementarity__national_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_17_complementarity__national_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_17_complementarity__national_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.62 because the reading's defining features — a high inadmissibility threshold with the proof burden inverted onto the Court — withdraw accountability access from every victim class except collapse cases, while the coordination core (anti-duplication, capacity incentives, cooperation preservation) remains real enough that the arrangement is not pure extraction. Suppression at 0.55 reflects structural-procedural closure: an adverse determination permanently closes the international route for a situation-class, and cooperation leverage punishes states that contest deference; the coercion is procedural and diplomatic rather than physical. Theater_ratio 0.28: sham-detection performs real screening (several state proceedings have been exposed as shams through exactly this machinery), but a growing share of admissibility litigation is positional — challenges filed to delay or politicize rather than to win. Accessibility_collapse 0.48: once a determination lands, alternatives (treaty-body referral, later reopening) exist but are slow and weak; collapse is substantial but incomplete because collapse-state cases retain full access. Resistance 0.55: sustained NGO, scholarly, and occasional chamber pressure for broader willingness tests meets equally sustained state-bloc defense. The measurement series share one time grid (t=0..24, step 4) across all three tracked metrics; suppression_requirement is authored because enforcement intensity is genuinely dynamic here — the sovereignty-defense machinery built up sharply through the Kenya-era confrontation (visible as the rise peaking near t=12-16), then plateaued as open non-cooperation normalized. Endpoint values equal the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   Seat divergence is extreme by construction of this reading. From the sovereignty_maximizing_states and national_judiciaries seats, the same rule computes as the founding bargain functioning correctly — subsidiarity respected, duplication avoided, domestic capacity honored; those seats classify coordination-forward. From atrocity_victims_weak_genuine_states and defendants_biased_national_trials, the identical rule operates as a locked door: the more competently their state stages selective justice, the less reachable accountability becomes — those seats classify extraction-forward, with amplified effective extraction because their exit is trapped. Collapsed_state_victim_communities occupy the sharpest vantage: powerless and trapped, yet structurally beneficiaries — the narrow reading is precisely what reserves the Court's capacity for them. The engine computes these divergent per-seat classifications from the declared structure; the authored claim adjudicates nothing.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionalities: national_judiciaries (constrained exit, institutional power) sit near the subsidized end; sovereignty_maximizing_states and incumbent_executive_networks add arbitrage-grade exit, placing them nearest the beneficiary pole, where the engine damps their effective extraction toward zero and can invert it into net subsidy. The payer declarations drive high directionalities, and trapped exit pins both victim classes near the full-target end; the regime's global scope further amplifies their effective extraction, since verifying 'genuineness' across hundreds of jurisdictions is exactly what large spatial scope makes difficult. Collapsed_state_victim_communities derive low d from their beneficiary declaration despite powerless and trapped atoms — the structural derivation reads the declaration before any canonical fallback. Suppression is authored as a raw structural property and is deliberately not scaled by power or scope; only extractiveness rides the directionality-and-scope computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling a permanent court with state sovereignty — is contested, not dead: sovereignty concerns remain live for a large state bloc even as accountability-first constituencies read the original problem as solved in form only. Because the status is contested rather than dead, no mandatrophy_resolved flag is declared and the arrangement shows no drift toward inertial maintenance: admissibility determinations still bind real cases and real victims. The tangled_rope claim prevents two symmetric mislabelings. Calling this pure rope would erase the victim-set restriction — the asymmetric transfer through which weak-genuine states convert deference into impunity space. Calling it pure snare would erase the load-bearing coordination without which the Court loses cooperation, funding, and ultimately its collapse-state caseload. The classification holds both halves visible and lets the temporal record adjudicate their balance; the rising extractiveness series with plateauing enforcement is the signature worth watching.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_delta,
    'This constraint is the national_primacy_reading of the article_17_complementarity kernel; which structural features would differ under the sibling international_oversight_reading?',
    'Authoring the sibling story and comparing victim sets, burden allocation, and computed per-seat types across both files; the comparison happens between stories, never by revising metrics within this one.',
    'Under the sibling reading the victim set expands beyond complete judicial collapse, the genuineness burden shifts onto states, and the arrangement computes more demanding at state seats and more accessible at victim seats — a different constraint, not a different verdict on this one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_delta, conceptual, 'Reading-indexed classification: the sibling reading changes the constraint itself, not merely the judgment on it.').

omega_variable(
    genuineness_line_drawing,
    'Where is the operational boundary between ''weak-but-genuine'' national proceedings and sham proceedings, and who draws it in practice?',
    'Comparative doctrinal analysis of decided admissibility determinations and challenges, tracing which evidentiary markers of independence and good faith the chambers accepted or rejected.',
    'A stricter genuineness test widens the internationally reachable victim set and lowers measured extraction; the current permissive line confines exclusion to weak-but-genuine states and sustains the authored extraction level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuineness_line_drawing, conceptual, 'The sham/genuine boundary is contestable, outcome-determinative, and drawn by the very institution the reading burdens.').

omega_variable(
    ability_will_masquerade,
    'Do weak-but-genuine national proceedings reflect genuine capacity limits, or strategic unwillingness presented as incapacity?',
    'Forensic audit comparing the scope of national prosecutions against documented command-responsibility chains and audited trial capacity in the same period.',
    'If unwillingness routinely masquerades as inability, effective extraction exceeds the authored value for affected victim classes and the arrangement trends toward extraction-dominance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ability_will_masquerade, empirical, 'Capacity-versus-will conflation inside the incapacity prong of the allocation rule.').

omega_variable(
    cooperation_subsidy_valence,
    'Does preserving state cooperation by keeping international reach narrow function as a subsidy for future victim access, or as bargaining currency states trade against accountability?',
    'Tracking cooperation episodes (warrant executions, transfers, evidence-sharing) against contemporaneous admissibility concessions across the interval.',
    'If cooperation reliably converts into deferred or diluted cases, the coordination component weakens and the balance shifts toward the extractive half; if cooperation consistently enables collapse-state prosecutions, the coordination component strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cooperation_subsidy_valence, empirical, 'The valence of the cooperation-for-reach trade sits at the heart of this reading''s rope/snare balance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_17_complementarity__national_primacy_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_17_complementarity__national_primacy_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement_basis(arti_tr_t0, observed).
narrative_ontology:measurement(arti_tr_t4, article_17_complementarity__national_primacy_reading, theater_ratio, 4, 0.16).
narrative_ontology:measurement_basis(arti_tr_t4, observed).
narrative_ontology:measurement(arti_tr_t8, article_17_complementarity__national_primacy_reading, theater_ratio, 8, 0.21).
narrative_ontology:measurement_basis(arti_tr_t8, observed).
narrative_ontology:measurement(arti_tr_t12, article_17_complementarity__national_primacy_reading, theater_ratio, 12, 0.26).
narrative_ontology:measurement_basis(arti_tr_t12, observed).
narrative_ontology:measurement(arti_tr_t16, article_17_complementarity__national_primacy_reading, theater_ratio, 16, 0.27).
narrative_ontology:measurement_basis(arti_tr_t16, observed).
narrative_ontology:measurement(arti_tr_t20, article_17_complementarity__national_primacy_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement_basis(arti_tr_t20, observed).
narrative_ontology:measurement(arti_tr_t24, article_17_complementarity__national_primacy_reading, theater_ratio, 24, 0.28).
narrative_ontology:measurement_basis(arti_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_17_complementarity__national_primacy_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(arti_be_t0, observed).
narrative_ontology:measurement(arti_be_t4, article_17_complementarity__national_primacy_reading, base_extractiveness, 4, 0.46).
narrative_ontology:measurement_basis(arti_be_t4, observed).
narrative_ontology:measurement(arti_be_t8, article_17_complementarity__national_primacy_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement_basis(arti_be_t8, observed).
narrative_ontology:measurement(arti_be_t12, article_17_complementarity__national_primacy_reading, base_extractiveness, 12, 0.57).
narrative_ontology:measurement_basis(arti_be_t12, observed).
narrative_ontology:measurement(arti_be_t16, article_17_complementarity__national_primacy_reading, base_extractiveness, 16, 0.6).
narrative_ontology:measurement_basis(arti_be_t16, observed).
narrative_ontology:measurement(arti_be_t20, article_17_complementarity__national_primacy_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement_basis(arti_be_t20, observed).
narrative_ontology:measurement(arti_be_t24, article_17_complementarity__national_primacy_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement_basis(arti_be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_17_complementarity__national_primacy_reading, suppression_requirement, 0, 0.34).
narrative_ontology:measurement_basis(arti_su_t0, observed).
narrative_ontology:measurement(arti_su_t4, article_17_complementarity__national_primacy_reading, suppression_requirement, 4, 0.38).
narrative_ontology:measurement_basis(arti_su_t4, observed).
narrative_ontology:measurement(arti_su_t8, article_17_complementarity__national_primacy_reading, suppression_requirement, 8, 0.5).
narrative_ontology:measurement_basis(arti_su_t8, observed).
narrative_ontology:measurement(arti_su_t12, article_17_complementarity__national_primacy_reading, suppression_requirement, 12, 0.56).
narrative_ontology:measurement_basis(arti_su_t12, observed).
narrative_ontology:measurement(arti_su_t16, article_17_complementarity__national_primacy_reading, suppression_requirement, 16, 0.56).
narrative_ontology:measurement_basis(arti_su_t16, observed).
narrative_ontology:measurement(arti_su_t20, article_17_complementarity__national_primacy_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement_basis(arti_su_t20, observed).
narrative_ontology:measurement(arti_su_t24, article_17_complementarity__national_primacy_reading, suppression_requirement, 24, 0.55).
narrative_ontology:measurement_basis(arti_su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_17_complementarity__national_primacy_reading, resource_allocation).
narrative_ontology:affects_constraint(article_17_complementarity__national_primacy_reading, article_17_complementarity__international_oversight_reading).

% DUAL FORMULATION NOTE:
% 'Article 17 complementarity' is a colloquial label covering two structurally distinct arrangements: the national-primacy instantiation (this file — high threshold, Court-carried burden, collapse-only victims, ε ≈ 0.62) and the international-oversight instantiation (sibling file — low threshold, state-carried burden, impunity-wide victims, materially different ε and beneficiary structure). Per the ε-invariance principle these are two constraints sharing a kernel, modeled as two stories linked through affects_constraints; pressure runs in both directions (each reading's victories reshape the other's operating environment) without either foreclosing the other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
