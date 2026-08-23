% ============================================================================
% CONSTRAINT STORY: constitutional_secularism__strict_neutrality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_secularism__strict_neutrality_reading, []).

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
 *   constraint_id: constitutional_secularism__strict_neutrality_reading
 *   human_readable: Strict Neutrality Secularism — Equidistant State Settlement
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   A constitutional settlement binds the state to equidistance from all
 *   religions: no establishment, no preferential treatment, no interference
 *   in doctrine, worship, or communal law. This file instantiates the
 *   strict_neutrality_reading of the constitutional_secularism kernel and is
 *   authored as a clean, epsilon-invariant constraint for that reading alone
 *   — the sibling readings (principled_intervention_reading,
 *   reformist_reading) are separate constraints reached through
 *   network.affects_constraints, and the kernel contest is carried in the
 *   omega variables and kernel_context rather than inside the constraint
 *   body. The scenario is a composite of disestablishment-style regimes
 *   rather than one named jurisdiction; scores are authored for the generic
 *   settlement. Structurally the settlement does two things at once: it
 *   manufactures an enforceable mutual-assurance good (every community is
 *   protected from establishment by rivals and from state meddling) and it
 *   prices that good in a specific currency — the state's protective capacity
 *   toward people inside communities, withheld precisely where communal
 *   authority is most absolute. Epsilon's referent is the standing
 *   strict-neutrality arrangement as operated, assessed by this reading's own
 *   lights: the reading endorses equidistance and still concedes the
 *   withheld-protection price, which is why the sibling readings exist at
 *   all. KEY AGENTS (by structural relationship): -
 *   constitutional_court_system: Agenda setter (institutional/constrained) —
 *   administers the settlement through adjudication; its precedents are the
 *   operating law - religious_minority_communities: Primary beneficiary
 *   (organized/constrained) — holds the enforceable assurance against
 *   establishment and interference - religious_community_leaderships:
 *   Beneficiary and receipt seat (organized/constrained) — collects insulated
 *   jurisdiction over members' family and communal life -
 *   majority_religious_establishment: Net payer with residual default-norm
 *   benefits (powerful/constrained) - women_under_religious_personal_law:
 *   Primary target (powerless/trapped) — bears the withheld-protection cost -
 *   religious_dissenters_and_apostates: Target (powerless/trapped) — the exit
 *   act itself goes unprotected - internal_reform_movements: Target
 *   (moderate/constrained) — stripped of the state lever -
 *   individual_believers, nonbelieving_citizens: Diffuse beneficiaries
 *   (moderate/constrained) - elected_branches_of_government: Bound
 *   administrator-payer (institutional/constrained) -
 *   comparative_constitutional_observers: Analytical observer
 *   (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_secularism__strict_neutrality_reading, 0.52).
domain_priors:suppression_score(constitutional_secularism__strict_neutrality_reading, 0.47).
domain_priors:theater_ratio(constitutional_secularism__strict_neutrality_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, suppression_requirement, 0.47).
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_secularism__strict_neutrality_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_secularism__strict_neutrality_reading, "Strict Neutrality Secularism — Equidistant State Settlement").
narrative_ontology:topic_domain(constitutional_secularism__strict_neutrality_reading, "constitutional/political").

domain_priors:requires_active_enforcement(constitutional_secularism__strict_neutrality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_secularism__strict_neutrality_reading, 'bd95cc3a-9f10-4ba1-822d-e47d3690393d').
narrative_ontology:cs_kernel_codification('bd95cc3a-9f10-4ba1-822d-e47d3690393d', fixed_text).
narrative_ontology:cs_authority_grounding('bd95cc3a-9f10-4ba1-822d-e47d3690393d', lineage).
narrative_ontology:cs_interpretation_layer_present('bd95cc3a-9f10-4ba1-822d-e47d3690393d').
narrative_ontology:cs_reading_relation('bd95cc3a-9f10-4ba1-822d-e47d3690393d', constitutional_secularism__principled_intervention_reading, forecloses).
narrative_ontology:cs_reading_relation('bd95cc3a-9f10-4ba1-822d-e47d3690393d', constitutional_secularism__reformist_reading, forecloses).
narrative_ontology:cs_axiom('bd95cc3a-9f10-4ba1-822d-e47d3690393d', foundational, equal_distance_owed_to_all_religions).
narrative_ontology:cs_axiom_status(equal_distance_owed_to_all_religions, holdable).
narrative_ontology:cs_axiom_grounding('bd95cc3a-9f10-4ba1-822d-e47d3690393d', equal_distance_owed_to_all_religions, deontological).
narrative_ontology:cs_axiom('bd95cc3a-9f10-4ba1-822d-e47d3690393d', secondary, prohibition_on_religious_preference).
narrative_ontology:cs_axiom_status(prohibition_on_religious_preference, holdable).
narrative_ontology:cs_axiom_grounding('bd95cc3a-9f10-4ba1-822d-e47d3690393d', prohibition_on_religious_preference, conventional).
narrative_ontology:cs_reference_frame('bd95cc3a-9f10-4ba1-822d-e47d3690393d', strict_equidistant_restraint).
narrative_ontology:cs_drift_state('bd95cc3a-9f10-4ba1-822d-e47d3690393d', contemporary_accommodation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bd95cc3a-9f10-4ba1-822d-e47d3690393d', '').
narrative_ontology:cs_kernel_id(constitutional_secularism__strict_neutrality_reading, constitutional_secularism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_secularism__strict_neutrality_reading, religious_minority_communities).
narrative_ontology:constraint_beneficiary(constitutional_secularism__strict_neutrality_reading, religious_community_leaderships).
narrative_ontology:constraint_beneficiary(constitutional_secularism__strict_neutrality_reading, individual_believers).
narrative_ontology:constraint_beneficiary(constitutional_secularism__strict_neutrality_reading, nonbelieving_citizens).
narrative_ontology:constraint_victim(constitutional_secularism__strict_neutrality_reading, women_under_religious_personal_law).
narrative_ontology:constraint_victim(constitutional_secularism__strict_neutrality_reading, religious_dissenters_and_apostates).
narrative_ontology:constraint_victim(constitutional_secularism__strict_neutrality_reading, internal_reform_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_secularism__strict_neutrality_reading, majority_religious_establishment).
narrative_ontology:constraint_victim(constitutional_secularism__strict_neutrality_reading, majority_religious_establishment).
narrative_ontology:constraint_victim(constitutional_secularism__strict_neutrality_reading, elected_branches_of_government).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicates disputes over the state's religious conduct: strikes down laws conferring preference on any confession and refuses petitions asking the state to act inside religious affairs. Gains arbitral authority from being the settlement's arbiter; bears the legitimacy cost of every ruling that disappoints a community. Cannot abandon the arrangement short of constitutional amendment; its own precedents are the arrangement's operating law.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, constitutional_court_system, agenda_setter,
    institutional, generational, constrained, national).

% Worship, educate, and run communal institutions under an enforceable guarantee that the state will neither fund a rival confession nor regulate their internal life. That guarantee is the arrangement's core product. Leaving the jurisdiction is possible in principle but severs the community ties the guarantee protects, so in practice they stay and defend the arrangement.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, religious_minority_communities, beneficiary,
    organized, generational, constrained, national).

% Clergy, councils, and personal-law boards hold governing authority over members' family life, inheritance, schooling, and endowments. Every increment of state distance enlarges the domain they administer without external review. They speak for their communities in constitutional negotiations and collect the deference paid to communal self-governance.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, religious_community_leaderships, beneficiary,
    organized, generational, constrained, national).

% Once held preferred access to public institutions and still supplies the demographic and electoral center of gravity. Renouncing preference cost it direct subsidy and symbolic primacy; what remains is the persistence of its norms as the surrounding cultural default, which formal equidistance does not displace. It funds much of the litigation testing the arrangement's boundaries.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, majority_religious_establishment, payer,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_secularism__strict_neutrality_reading, majority_religious_establishment, beneficiary).

% Live under family-law regimes administered by their religious communities — marriage, divorce, custody, inheritance rules they did not choose and cannot veto. The settlement forbids the state from reaching into those regimes even to protect them, so their avenue of redress runs through community tribunals staffed by the authorities whose rules they contest. Exit means losing family, community, and often livelihood at once.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, women_under_religious_personal_law, payer,
    powerless, biographical, trapped, national).

% Members who reject or leave the faith find that the settlement shields the community's authority over them from state second-guessing: shunning, the civil effects of excommunication, and conversion-hostile family law all stand inside the protected zone. Their defining act — leaving — is precisely what the settlement will not assist.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, religious_dissenters_and_apostates, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(constitutional_secularism__strict_neutrality_reading, religious_dissenters_and_apostates, excluded).

% Reformers inside each tradition seek leverage to change practices from within; the settlement removes the largest available lever, since the state will not condition recognition, funding, or legal support on reform. They campaign through persuasion and community politics alone, against authorities the settlement has insulated.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, internal_reform_movements, payer,
    moderate, biographical, constrained, national).

% Practice, convert, and observe without registering with or answering to the state. The guarantee is diffuse and personally real; its costs reach them only indirectly, through the communal authorities it preserves.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, individual_believers, beneficiary,
    moderate, biographical, constrained, national).

% Hold office, serve on juries, and raise children without religious tests or confessional instruction backed by the state. Equidistance is what keeps the public square open to them; they have no communal authority of their own to fall back on if it fails.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, nonbelieving_citizens, beneficiary,
    moderate, biographical, constrained, national).

% Legislatures and executives write the statutes the settlement governs and periodically test its edges with accommodations, exemptions, and symbolic measures. Each branch has lost policy instruments the settlement fences off — establishment, confessional programming, intervention in communal law — while retaining day-to-day administration of everything else.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, elected_branches_of_government, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_secularism__strict_neutrality_reading, elected_branches_of_government, agenda_setter).

% Scholars and monitoring bodies compare how different polities operationalize state-religion distance, documenting which settlements protect whom and at whose expense. They hold no stake in any single settlement and publish the cross-jurisdictional record the other seats argue from.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, comparative_constitutional_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_secularism__strict_neutrality_reading, religious_community_leaderships).
narrative_ontology:fixing_cost_class(constitutional_secularism__strict_neutrality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the plural-polity problem: how adherents of many confessions share one state without the state becoming a sectarian weapon. By binding the state to equidistance, it gives every community an enforceable assurance against establishment by rivals and against state meddling in doctrine, worship, and communal law.
% TRANSFER_FUNCTION: Transfers jurisdiction over religious life — doctrine, worship, personal law, endowments, schooling — from state institutions to community self-governance; correspondingly withholds state protective capacity from individuals subject to communal authority; delivers inter-confessional peace to the polity at large.
% ABSENT_VOICES: Intra-community dissenters — women governed by discriminatory personal law, apostates, heterodox members — object through representatives who do not share their interests: community leadership bargains as 'the community,' and the neutrality bargain treats that voice as consent. They sit inside the very communities whose autonomy the settlement guarantees, with no seat at the constitutional table except as litigants after harm.
% DISAPPEARANCE_RATIONALE: Confessional policy would become the immediate prize of electoral and sectarian competition: establishment struggles, religious patronage, and counter-mobilization would rearrange party systems and inter-communal relations within a few electoral cycles, and the mutual-assurance structure every community relies on would collapse simultaneously.
% FOUNDING_PROBLEM: Post-Reformation confessional warfare and the founding-era problem of state capture by an established church: how a religiously plural polity shares one sovereign without the state becoming an instrument of one confession against the rest.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the beneficiary set: the historical record of confessional wars and disestablishment struggles documented by historians independent of any current religious beneficiary; comparative-constitutional scholarship on why plural polities adopt neutrality clauses; and the payer seats themselves — internal reform movements acknowledge the peace function is real even while contesting the non-intervention cost. Community leaderships also attest the founding problem, but they are beneficiaries; the corroboration that counts comes from historians, comparativists, and the settlement's own targets.
narrative_ontology:disappearance_verdict(constitutional_secularism__strict_neutrality_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_secularism__strict_neutrality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_secularism__strict_neutrality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_secularism__strict_neutrality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_secularism__strict_neutrality_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_secularism__strict_neutrality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_secularism__strict_neutrality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_secularism__strict_neutrality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.52: the settlement takes no money or labor; its extraction is jurisdictional — it allocates governance of intimate life to communal authorities and withholds state protection from the members least able to contest those authorities. The cost is real, concentrated, and identifiable, but bounded by the breadth of the coordination good everyone shares. Suppression 0.47: the mechanism is structural, not interpersonal — constitutional foreclosure of the state's intervention and preference option-space, enforced by courts against legislative deviation; no person is coerced by the settlement directly, and the decline from the mid-century enforcement peak reflects accommodation-era retrenchment rather than normalization (see the enforcement_retrenchment_direction omega). Theater 0.28: enforcement is predominantly functional — real doctrines, real vetoes — with a ceremonial stratum of neutrality language coexisting alongside informal accommodations. Accessibility_collapse 0.58: within the legal order the alternatives (establishment, intervention) collapse almost completely — pursuing them requires amendment or doctrinal reversal — but they remain politically live, keeping the figure well below natural-law levels. Resistance 0.60: continuous litigation and political pressure from accommodation-seekers, establishmentarians, and reform advocates; the settlement is defended, not self-executing. The temporal series share one grid (1791-2026, seven points, all three metrics authored at every point); suppression_requirement is tracked because enforcement-capacity change is the settlement's traced dynamic — build-up through the mid-century disestablishment era, partial retrenchment since. Coalition note: the powerless payer seats are not structurally doomed — an alliance of internal reform movements with the women and dissenters bearing the withheld-protection cost is the principal internal threat to the settlement, and its absence so far is a fact about mobilization, not structure.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the court seat the settlement is a legitimate administration it conducts; from the minority-community and believer seats it is a protection they hold; from the leadership seat it is an enlarged, unreviewed domain; from the women-under-personal-law and dissenter seats the identical structure operates as abandonment — the state watching from a distance it built. The majority establishment seat is genuinely split: it paid establishment primacy and still collects default-norm persistence. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: religious_minority_communities, individual_believers, and nonbelieving_citizens derive low d (subsidized by the assurance good); religious_community_leaderships derive the lowest d — they receive jurisdiction with every increment of state distance and face no countervailing extraction. Victims: women_under_religious_personal_law and religious_dissenters_and_apostates derive near-full-target d — powerless, trapped (exit means losing family, community, and livelihood simultaneously), and the settlement's specific refusal aims at their situation. internal_reform_movements derive high d with somewhat more mobility (they can still operate through persuasion). majority_religious_establishment sits mid-scale: formally a payer (renounced preference) with substantial residual benefit (default-norm persistence), encoded by its dual role. elected_branches_of_government sit mid-high: they lost policy instruments but still administer and periodically test the settlement's edges. constitutional_court_system sits near symmetric: it expends legitimacy enforcing the settlement and collects arbitral authority from doing so. No directionality overrides were needed: the beneficiary/victim declarations plus exit atoms produce the correct ordering, and the one potentially ambiguous seat (majority establishment) is differentiated by its dual role rather than by override.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards both misreadings. Calling the settlement pure coordination would erase the identifiable payers whose protection is the currency spent to buy inter-communal peace; calling it pure extraction would erase the assurance good every seat — including the payers' own communities — depends on and which no alternative arrangement currently supplies. Tangled rope holds both halves. On the genealogy interview: the founding problem (confessional state capture) is live, attested from outside the beneficiary set, and the disappearance verdict is world_rearranges — status=live crossed with world_rearranges raises no zombie flag, and no mandatrophy resolution is declared. The settlement has not outlived its mandate; it is executing its mandate at a price its own reading concedes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This constraint is one reading of the constitutional_secularism kernel — the strict_neutrality_reading. What would the sibling readings change structurally, and where exactly is the disagreement located?',
    'Comparative constitutional analysis: jurisdictions and courts adopting each reading expose the structural deltas — principled_intervention shrinks the withheld-protection victim set by licensing state entry for reform; reformist_reading inverts the autonomy priority altogether. The disagreement is located in a single element: whether state restraint toward religious affairs is categorical or defeasible when communal practices harm members.',
    'If the categorical premise yields, this constraint dissolves into one of its siblings: the victim set contracts (intervention) or the beneficiary structure inverts (reformist duty), and classification recomputes on the sibling''s structure rather than this one''s.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer structure: one reading of the constitutional_secularism kernel; the siblings alter the categorical-restraint premise.').

omega_variable(
    formal_equidistance_vs_majority_default,
    'Does formal equidistance produce substantively equal treatment when one community''s norms saturate the surrounding culture — holidays, oaths, symbols, scheduling — so that ''neutral'' defaults track the majority?',
    'Cross-jurisdictional outcome comparison between polities enforcing formal equidistance and those enforcing substantive equal-status tests: measure minority members'' realized burdens (accommodation denials, default-norm exposure) under each.',
    'If formal equidistance masks majority defaults, effective extraction on the minority and nonbeliever seats is higher than the authored metrics suggest and the settlement drifts toward extraction-dominant operation for those seats; if defaults wash out, the coordination reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formal_equidistance_vs_majority_default, empirical, 'Whether the structural delta ''vulnerability to majority norms'' is intrinsic to formal equidistance or contingent on local demographics.').

omega_variable(
    withheld_protection_consent_status,
    'Do the people bearing the settlement''s concentrated cost — women under communal personal law, dissenters — experience the withheld protection as extraction, or do they endorse the autonomy settlement that prices their protection?',
    'Litigation and survey records: track whether internal challengers seek state intervention when offered the choice, and how intra-community opinion divides on inviting state oversight of personal law.',
    'If most affected members endorse the settlement, the victims array is over-scoped and the extraction estimate falls toward a coordination-cost reading; if challengers systematically seek intervention, the authored victim structure stands confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(withheld_protection_consent_status, empirical, 'Whether the concentrated cost-bearers consent to the trade the settlement makes on their behalf.').

omega_variable(
    enforcement_retrenchment_direction,
    'Is the recent decline in enforcement intensity (suppression_requirement falling from its mid-century peak) consolidation of the settlement into a self-sustaining norm, or erosion under accumulating accommodation pressure?',
    'Track the next two decades of apex-court rulings, constitutional amendments, and statutory accommodations: consolidation shows stable outcomes despite lower enforcement effort; erosion shows widening breaches of equidistance.',
    'Erosion predicts reassertion of majority defaults and rising minority exposure (the structural delta worsening); consolidation stabilizes the settlement''s current profile. The two paths date any type transition very differently.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_retrenchment_direction, empirical, 'Direction of the enforcement trajectory after the mid-century peak.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_secularism__strict_neutrality_reading, 1791, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t1791, constitutional_secularism__strict_neutrality_reading, theater_ratio, 1791, 0.12).
narrative_ontology:measurement(cons_tr_t1840, constitutional_secularism__strict_neutrality_reading, theater_ratio, 1840, 0.15).
narrative_ontology:measurement(cons_tr_t1890, constitutional_secularism__strict_neutrality_reading, theater_ratio, 1890, 0.18).
narrative_ontology:measurement(cons_tr_t1940, constitutional_secularism__strict_neutrality_reading, theater_ratio, 1940, 0.22).
narrative_ontology:measurement(cons_tr_t1970, constitutional_secularism__strict_neutrality_reading, theater_ratio, 1970, 0.25).
narrative_ontology:measurement(cons_tr_t2000, constitutional_secularism__strict_neutrality_reading, theater_ratio, 2000, 0.27).
narrative_ontology:measurement(cons_tr_t2026, constitutional_secularism__strict_neutrality_reading, theater_ratio, 2026, 0.28).

% Extraction over time
narrative_ontology:measurement(cons_be_t1791, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 1791, 0.4).
narrative_ontology:measurement(cons_be_t1840, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 1840, 0.42).
narrative_ontology:measurement(cons_be_t1890, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 1890, 0.44).
narrative_ontology:measurement(cons_be_t1940, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 1940, 0.46).
narrative_ontology:measurement(cons_be_t1970, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 1970, 0.49).
narrative_ontology:measurement(cons_be_t2000, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 2000, 0.51).
narrative_ontology:measurement(cons_be_t2026, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 2026, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t1791, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 1791, 0.25).
narrative_ontology:measurement(cons_su_t1840, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 1840, 0.3).
narrative_ontology:measurement(cons_su_t1890, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 1890, 0.38).
narrative_ontology:measurement(cons_su_t1940, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 1940, 0.5).
narrative_ontology:measurement(cons_su_t1970, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 1970, 0.55).
narrative_ontology:measurement(cons_su_t2000, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 2000, 0.5).
narrative_ontology:measurement(cons_su_t2026, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 2026, 0.47).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_secularism__strict_neutrality_reading, identity_coordination).
narrative_ontology:affects_constraint(constitutional_secularism__strict_neutrality_reading, principled_intervention_reading).
narrative_ontology:affects_constraint(constitutional_secularism__strict_neutrality_reading, reformist_reading).

% DUAL FORMULATION NOTE:
% 'Constitutional secularism' is a colloquial label covering three structurally distinct settlements with different victim sets and different epsilon values: this file authors the strict_neutrality_reading (equidistance, non-interference); principled_intervention_reading permits state entry into religious affairs for reform and protection of weaker sections; reformist_reading imposes an affirmative state duty to eliminate oppressive practices. The strict reading is the upstream baseline: interventionist and reformist arrangements are typically justified as departures from it, citing harms the strict settlement leaves unremedied. Each reading is authored as its own constraint with its own beneficiaries, victims, and epsilon; the epsilon differences are the point of the decomposition, not a measurement artifact.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
