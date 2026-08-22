% ============================================================================
% CONSTRAINT STORY: waitangi_sovereignty_allocation__partnership_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_waitangi_sovereignty_allocation__partnership_reading, []).

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
 *   constraint_id: waitangi_sovereignty_allocation__partnership_reading
 *   human_readable: Treaty of Waitangi Partnership Doctrine (Partnership Reading)
 *   domain: constitutional/indigenous rights/post-colonial governance
 *
 * SUMMARY:
 *   The Treaty of Waitangi 1840 survives in two texts that disagree about
 *   what was ceded and what was retained. The partnership reading —
 *   crystallized judicially in NZ Māori Council v Attorney-General (1987) and
 *   operationalized through the Waitangi Tribunal (1975), section 9 of the
 *   State-Owned Enterprises Act (1985), and the subsequent settlements regime
 *   — holds that the Treaty created a continuing bilateral compact obligating
 *   the Crown to good faith consultation and active protection of Māori
 *   interests. This story authors THAT reading as a single ε-invariant
 *   constraint: the standing arrangement of
 *   principles-doctrine-plus-settlements, assessed by the reading's own
 *   lights. The claim/metric split is deliberate: the reading is CLAIMED as
 *   tangled_rope (both a genuine coordination function and asymmetric
 *   extraction), and the metrics are authored descriptively of the
 *   arrangement's actual operation — the engine computes per-seat
 *   classifications from the structural data; nothing here is tuned to a
 *   predicted verdict.
 *
 * KEY AGENTS:
 *   - crown_executive_government: agenda-setter and net beneficiary (institutional/constrained) — administers the doctrine, controls settlement mandates, captures legitimacy and closure while funding redress
 *   - maori_collectives_iwi_hapu: primary beneficiary (organized/identity_locked) — hold consultation rights, settlement assets, and co-governance seats; the relationship is constitutive, so exit is unavailable
 *   - maori_claimant_groups: principal payers (organized/identity_locked) — surrendered historical claims full-and-final at fractional quantum and bear the burden of proof in inquiries
 *   - private_resource_users: secondary payers (powerful/constrained) — bear consultation and co-governance compliance costs
 *   - nz_judiciary: enforcement agenda-setter (institutional/constrained) — authored the doctrine and enforces principles where statutes incorporate them
 *   - waitangi_tribunal: recommendatory observer (institutional/analytical) — investigates breaches and defines prejudice without power to compel
 *   - territorial_authorities: payers (institutional/constrained) — administer co-governance and consultation duties locally
 *   - general_non_maori_public: dual beneficiary/payer (organized/constrained) — enjoys stability and legitimacy, funds settlements as taxpayers
 *   - un_treaty_bodies: external observer (institutional/analytical) — review performance against UNDRIP with reputational force only
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(waitangi_sovereignty_allocation__partnership_reading, 0.55).
domain_priors:suppression_score(waitangi_sovereignty_allocation__partnership_reading, 0.4).
domain_priors:theater_ratio(waitangi_sovereignty_allocation__partnership_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__partnership_reading, tangled_rope).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__partnership_reading, "Treaty of Waitangi Partnership Doctrine (Partnership Reading)").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__partnership_reading, "constitutional/indigenous rights/post-colonial governance").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__partnership_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__partnership_reading, 'b99297cb-fee9-4a6b-ad54-7428adfa2e62').
narrative_ontology:cs_kernel_codification('b99297cb-fee9-4a6b-ad54-7428adfa2e62', fixed_text).
narrative_ontology:cs_authority_grounding('b99297cb-fee9-4a6b-ad54-7428adfa2e62', lineage).
narrative_ontology:cs_interpretation_layer_present('b99297cb-fee9-4a6b-ad54-7428adfa2e62').
narrative_ontology:cs_reading_relation('b99297cb-fee9-4a6b-ad54-7428adfa2e62', waitangi_sovereignty_allocation__crown_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('b99297cb-fee9-4a6b-ad54-7428adfa2e62', waitangi_sovereignty_allocation__rangatiratanga_reading, influences).
narrative_ontology:cs_axiom('b99297cb-fee9-4a6b-ad54-7428adfa2e62', foundational, treaty_continuing_mutual_obligation).
narrative_ontology:cs_axiom_status(treaty_continuing_mutual_obligation, holdable).
narrative_ontology:cs_axiom_grounding('b99297cb-fee9-4a6b-ad54-7428adfa2e62', treaty_continuing_mutual_obligation, conventional).
narrative_ontology:cs_axiom('b99297cb-fee9-4a6b-ad54-7428adfa2e62', foundational, active_protection_fiduciary_duty).
narrative_ontology:cs_axiom_status(active_protection_fiduciary_duty, holdable).
narrative_ontology:cs_axiom_grounding('b99297cb-fee9-4a6b-ad54-7428adfa2e62', active_protection_fiduciary_duty, deontological).
narrative_ontology:cs_reference_frame('b99297cb-fee9-4a6b-ad54-7428adfa2e62', good_faith_mutual_partnership_frame).
narrative_ontology:cs_drift_state('b99297cb-fee9-4a6b-ad54-7428adfa2e62', contemporary_retrenchment_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b99297cb-fee9-4a6b-ad54-7428adfa2e62', '').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__partnership_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__partnership_reading, maori_collectives_iwi_hapu).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__partnership_reading, crown_executive_government).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__partnership_reading, maori_claimant_groups).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__partnership_reading, private_resource_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__partnership_reading, general_non_maori_public).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__partnership_reading, crown_executive_government).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__partnership_reading, territorial_authorities).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__partnership_reading, general_non_maori_public).
narrative_ontology:constraint_vindicates(waitangi_sovereignty_allocation__partnership_reading, treaty_principles_doctrine).
narrative_ontology:constraint_vindicates(waitangi_sovereignty_allocation__partnership_reading, fiduciary_relational_good_faith).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the terms on which Treaty principles apply: decides which bills carry principles clauses, issues settlement negotiation mandates, determines when and how consultation occurs, and responds to Tribunal findings. Funds settlement packages from the public purse and receives in return the closure of historical claims, international legitimacy, and continued governing authority over nearly the entire national land area. Its discretion is bounded only where Parliament has chosen to bound it; it can propose legislation altering the doctrine's reach, as the 2024-25 Treaty Principles Bill episode showed.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, crown_executive_government, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__partnership_reading, crown_executive_government, payer).

% Hold recognised interests in lands, waters, and taonga backed by consultation rights, Tribunal access, and post-settlement governance seats on rivers, parks, and conservation estates. Post-settlement iwi operate settlement assets and sit on co-governance bodies. Their relationship to the land and to the Crown is constitutive of who they are — whakapapa and whenua are not assets that can be traded away — so leaving the arrangement is not an available move in any meaningful sense.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, maori_collectives_iwi_hapu, beneficiary,
    organized, civilizational, identity_locked, national).

% Brought the historical grievances that drove the inquiry and settlement machinery. In exchange for negotiated packages typically valued far below tribunal-assessed losses, they executed deeds settling claims finally and forever, binding descendants not yet born. They also carry the evidential burden in inquiries and the ongoing workload of consultation participation, often at their own expense.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, maori_claimant_groups, payer,
    organized, generational, identity_locked, regional).

% Infrastructure builders, energy companies, farmers, and fishers whose projects and operations intersect Māori interests. They fund and undergo consultation processes, accommodate co-governance arrangements over water and conservation estates, and absorb schedule risk while decisions pend. Their recourse is compliance, litigation, or relocating investment.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, private_resource_users, payer,
    powerful, biographical, constrained, national).

% Authored the modern doctrine in the 1987 Lands Case, articulating partnership, active protection, and reasonable cooperation as principles guiding Crown action. Enforces those principles wherever Parliament has written them into statute, while repeatedly affirming that Parliament remains free to legislate inconsistently with the Treaty.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, nz_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Registers claims, hears evidence, and reports on whether Crown actions breached the Treaty and caused prejudice. Its recommendations bind the Crown only in narrow statutory circumstances; mostly it persuades, defines the historical record, and supplies the findings on which settlement negotiations proceed.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, waitangi_tribunal, observer,
    institutional, generational, analytical, national).

% Regional and city councils administering resource consenting, water infrastructure, and local planning under national statutes that increasingly require Māori participation in decisions. They staff and fund engagement processes and share decision forums with iwi appointees.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, territorial_authorities, payer,
    institutional, biographical, constrained, regional).

% Enjoys the civic peace and international standing the settlement programme purchases, and funds it as taxpayers. Most encounter the arrangement only through headlines about settlements, co-governance proposals, and periodic protest; their electoral weight periodically swings the doctrine's fortunes, as in the 2023 coalition's principles-referendum promise.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, general_non_maori_public, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__partnership_reading, general_non_maori_public, payer).

% Review New Zealand's performance against the Declaration on the Rights of Indigenous Peoples and treaty commitments, issuing concluding observations that carry no domestic legal force but shape international reputation and supply arguments to domestic advocates.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, un_treaty_bodies, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(waitangi_sovereignty_allocation__partnership_reading, crown_executive_government).
narrative_ontology:fixing_cost_class(waitangi_sovereignty_allocation__partnership_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Manages the continuing relationship between the Crown and Māori polities: channels disputes about lands, waters, and taonga into inquiry, consultation, and negotiation rather than extra-constitutional conflict, and supplies a shared doctrinal vocabulary under which two systems of authority can occupy one territory.
% TRANSFER_FUNCTION: Moves negotiated compensation — cash, land, commercial assets, apologies, co-governance seats — from the Crown to iwi claimant groups in exchange for full-and-final surrender of historical claims; moves procedural influence from unilateral Crown discretion toward shared processes; and moves legitimacy from a contested-conquest footing to a cession-and-partnership footing.
% ABSENT_VOICES: Holders of the rangatiratanga reading — rangatira descendants who regard Article II as retaining full authority and the principles doctrine as a domesticating translation — have no seat in settlement mandates or principles-definition; their objections surface in protests, minority Tribunal opinions, and scholarship. Unsettled claimants outside the Crown's negotiation window, and the generations bound by full-and-final deeds, are likewise unrepresented where the terms are set.
% DISAPPEARANCE_RATIONALE: Settled deeds would lose their interpretive foundation overnight; co-governance bodies over the Waikato River, Te Urewera, and the conservation estate would lapse into ordinary title disputes; roughly a hundred negotiated settlements would become contestable anew; and the Crown's authority would rest on bare majoritarian command contested by organised iwi — the constitutional question the arrangement manages would return in raw form.
% FOUNDING_PROBLEM: How a Crown asserting authority under a treaty whose two texts disagree could govern alongside Māori polities that never conceded unqualified cession — converting an ambiguous cession-and-guarantee document into administrable mutual obligations.
% FOUNDING_PROBLEM_CORROBORATION: Court of Appeal and Supreme Court jurisprudence, authored independently of Crown preference, treats the mutual-obligation question as unresolved and continuing; Waitangi Tribunal reports — frequently adverse to the Crown — document live breach and prejudice claims into the present decade; UN treaty-body reviews press the same questions from outside the domestic settlement; and Māori scholarship contests the doctrine precisely because the founding ambiguity remains open. No party to the dispute asserts the founding problem is closed.
narrative_ontology:disappearance_verdict(waitangi_sovereignty_allocation__partnership_reading, world_rearranges).
narrative_ontology:founding_problem_status(waitangi_sovereignty_allocation__partnership_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(waitangi_sovereignty_allocation__partnership_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(waitangi_sovereignty_allocation__partnership_reading, 'none', 1).
narrative_ontology:epsilon_provenance(waitangi_sovereignty_allocation__partnership_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(waitangi_sovereignty_allocation__partnership_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(waitangi_sovereignty_allocation__partnership_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(waitangi_sovereignty_allocation__partnership_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is 0.55 at interval end: the arrangement delivers real goods (transferred assets, co-governance seats, protected taonga, a peaceful dispute channel) yet the Crown retains governing authority over nearly the entire land area, consultation is advisory only, and settlements extinguish claims finally at quanta typically far below assessed loss. Suppression is 0.40 and is overwhelmingly STRUCTURAL rather than internalized: there is no alternative jurisdiction for Māori to exit into — the legal order is unitary — while current enforcement intensity is modest (no violent maintenance at the margin; the 1880s peak of war, confiscation, and the Native Land Court is long past). Theater is 0.34: consultation frequently ratifies predetermined decisions and anniversaries are ceremonial, but the Tribunal and the settlements machinery do real work. Accessibility_collapse is 0.42 — alternatives persist (ordinary courts, electoral politics, protest, international review) and the doctrine forecloses none of them completely. Resistance is 0.60 — sustained across 150 years: the 1975 Land March, Bastion Point, foreshore-and-seabed mobilisation, Ihumātao, and the 2024 hīkoi mō te Tiriti. The temporal series show a rise-collapse-recovery arc driven by political cycles (betrayal peak 1880s, doctrinal trough circa 2012, retrenchment uptick to 2025 as the Principles Bill episode signaled possible rollback) — the oscillation is a side effect of electoral turnover, not an intermittent-reinforcement mechanism. Coalition capacity is real: post-settlement iwi corporate structures give the payer seats organized power, which is why the victim seats are 'organized' rather than 'powerless'. Coordination type is identity_coordination because the function whose failure would recreate the founding problem is boundary-maintenance between two polities in one territory; the FNL gaming risk is acknowledged — the identity frame here is the thing being contested, and the coupling concern (costs concentrated on the identity-locked party) is exactly what the metrics flag rather than excuse.
 *
 * PERSPECTIVAL GAP:
 *   From the Crown seat the arrangement is generous statesmanship: billions transferred, co-governance granted, grievances heard. From the claimant-group seat it is a machine that converts unextinguished grievance into small final payments under the shadow of statutory limitation and full-and-final clauses. From the judiciary's seat it is a workable reconciliation of two irreconcilable texts. From the rangatiratanga holder's seat — outside this reading — it is the domestication of retained authority into consultation rights. One structure, four incompatible experiences; the engine computes the divergence from the structural data, and this story does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations map directly onto the seats: maori_collectives_iwi_hapu derive low directionality (the arrangement subsidizes them with rights, assets, and seats); crown_executive_government declares beneficiary but is genuinely dual — it funds settlements and absorbs consultation limits while capturing legitimacy, closure, and continued dominion, leaving it beneficiary-leaning but not fully subsidized; maori_claimant_groups and private_resource_users derive high directionality (they bear the fractional extinguishment and the compliance costs respectively); general_non_maori_public sits near symmetric (taxpayer cost against stability benefit). No directionality overrides are used: the derivation from declarations plus exit options produces the right qualitative ordering, and the Crown's mixed position is captured by its dual role rather than by a blunt per-power-atom override, which would also sweep in the judiciary, councils, and UN bodies.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two symmetrical mislabels. Calling the arrangement a rope (pure coordination) erases the fractional-extinguishment asymmetry — claims closed at cents-on-the-dollar while the underlying estate stays with the Crown. Calling it a snare erases the genuine coordination — real assets transferred, real co-governance seats, a channel that replaced armed conflict. Tangled rope holds both halves. On mandatrophy: the founding problem (ambiguous cession) is live, corroborated from outside the benefiting parties, so no zombie flag fires; but the consultation limb shows theater drift (0.26 to 0.34 over the last interval segment) — if theater_ratio crosses 0.5 the consultation limb trends inertial while the settlement limb remains functional, a partial-degradation signature worth watching rather than a whole-constraint verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This story instantiates the partnership_reading of the waitangi_sovereignty_allocation kernel. How would the constraint''s structure differ under the crown_sovereignty_reading (complete cession, Westminster supremacy) or the rangatiratanga_reading (retained tino rangatiratanga, Crown limited to kāwanatanga)?',
    'Comparative classification of the sibling stories: author each sibling as its own ε-invariant constraint and compare computed types, ε, and victim sets against this reading.',
    'Under the crown_sovereignty_reading the same history computes as settled law with negligible ongoing obligation (the principles doctrine would be gratuitous grace, not owed duty); under the rangatiratanga_reading the standing arrangement computes as usurpation with severe extraction and a far larger victim set. The partnership reading sits between: real obligation, partial performance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: which reading of the sovereignty-allocation kernel this constraint is, and what siblings would change.').

omega_variable(
    governing_text_ambiguity,
    'Which text of the Treaty governs — the English cession text or the Māori rangatiratanga text? The partnership reading exists only because both texts are treated as authentic and mutually qualifying.',
    'Legal-historical analysis of Hobson''s instructions, the oral assurances recorded at signing, the 1835 He Whakaputanga context, and the contra proferentem tradition favoring the indigenous text.',
    'If the Māori text is taken as governing, the partnership reading systematically understates what Māori retained, and the standing arrangement''s measured extraction rises sharply toward the rangatiratanga_reading''s profile; if the English text governs, the partnership reading overstates the obligation and collapses toward the crown_sovereignty_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(governing_text_ambiguity, conceptual, 'Whether the two-text discrepancy resolves toward cession, partnership, or retention.').

omega_variable(
    consultation_substance_vs_theater,
    'At the current margin, is consultation substantively effective or predominantly performative — does Māori input alter decisions, or ratify ones already made?',
    'Outcome-tracking of consultation episodes across consenting, conservation, and water-governance decisions: measure the rate at which submitted input changes the final decision.',
    'Rising theater in the consultation limb signals piton-direction drift inside the tangled rope even while the settlement limb stays functional; substantively effective consultation supports the coordination-function half of the classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consultation_substance_vs_theater, empirical, 'Whether the consultation limb performs real coordinative work or ceremonial work.').

omega_variable(
    settlement_quantum_adequacy,
    'Are settlement packages adequate redress or fractional extinguishment — how does the negotiated quantum compare to tribunal-assessed loss at comparable valuation?',
    'Economic-historical accounting comparing aggregate settlement value (including commercial redress and co-governance assets) against Tribunal prejudice findings and current-value estimates of confiscated and precariously purchased land.',
    'If settlements are fractional, the settlement mechanism is itself a value-transfer device operating inside the partnership frame — confirming the asymmetric-extraction half of the classification; if near-adequate, the arrangement trends toward genuine coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settlement_quantum_adequacy, empirical, 'Whether the redress machinery restores or extinguishes the underlying claim at a discount.').

omega_variable(
    entrenchment_trajectory,
    'Will the principles doctrine harden into entrenched, judicially enforceable limit independent of statutory incorporation, or will it remain subordinate to parliamentary sovereignty and vulnerable to political repeal?',
    'Constitutional development: Supreme Court treatment of the principles'' status, any entrenchment provision, and the political fate of principles-redefinition attempts such as the 2024-25 Treaty Principles Bill.',
    'Entrenchment would raise the arrangement''s suppression profile (a fight over a hardened limit) while securing the coordination function; doctrinal abandonment would collapse the constraint toward the crown_sovereignty_reading''s world and strand settled expectations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(entrenchment_trajectory, empirical, 'Future constitutional status of the principles doctrine relative to parliamentary supremacy.').

omega_variable(
    maori_identity_constitution,
    'How much of the Māori seats'' persistence in the arrangement rests on identity-constitution (whakapapa, whenua, taonga as constitutive of the self) rather than on calculated return?',
    'Counterfactual behavioral analysis: how iwi strategy changes when returns fall — continued engagement despite negative expected returns indicates identity-lock; exit or escalation indicates instrumental positioning.',
    'Identity-lock keeps the beneficiary and payer seats engaged regardless of returns, stabilizing the arrangement beyond what its delivery alone would sustain; if the identity frame broke, the extraction-bearing seats would escalate or exit and the arrangement''s equilibrium would fail quickly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maori_identity_constitution, conceptual, 'Identity-fusion mechanism binding Māori seats to the relationship.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(waitangi_sovereignty_allocation__partnership_reading, 1840, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(waitangi_partnership_tr_t1840, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 1840, 0.2).
narrative_ontology:measurement_basis(waitangi_partnership_tr_t1840, observed).
narrative_ontology:measurement(waitangi_partnership_tr_t1880, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 1880, 0.55).
narrative_ontology:measurement_basis(waitangi_partnership_tr_t1880, observed).
narrative_ontology:measurement(waitangi_partnership_tr_t1920, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 1920, 0.6).
narrative_ontology:measurement_basis(waitangi_partnership_tr_t1920, observed).
narrative_ontology:measurement(waitangi_partnership_tr_t1960, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 1960, 0.55).
narrative_ontology:measurement_basis(waitangi_partnership_tr_t1960, observed).
narrative_ontology:measurement(waitangi_partnership_tr_t1975, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 1975, 0.45).
narrative_ontology:measurement_basis(waitangi_partnership_tr_t1975, observed).
narrative_ontology:measurement(waitangi_partnership_tr_t1987, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 1987, 0.3).
narrative_ontology:measurement_basis(waitangi_partnership_tr_t1987, observed).
narrative_ontology:measurement(waitangi_partnership_tr_t2000, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 2000, 0.28).
narrative_ontology:measurement_basis(waitangi_partnership_tr_t2000, observed).
narrative_ontology:measurement(waitangi_partnership_tr_t2012, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 2012, 0.26).
narrative_ontology:measurement_basis(waitangi_partnership_tr_t2012, observed).
narrative_ontology:measurement(waitangi_partnership_tr_t2025, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 2025, 0.34).
narrative_ontology:measurement_basis(waitangi_partnership_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(waitangi_partnership_be_t1840, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 1840, 0.35).
narrative_ontology:measurement_basis(waitangi_partnership_be_t1840, observed).
narrative_ontology:measurement(waitangi_partnership_be_t1880, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 1880, 0.86).
narrative_ontology:measurement_basis(waitangi_partnership_be_t1880, observed).
narrative_ontology:measurement(waitangi_partnership_be_t1920, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 1920, 0.8).
narrative_ontology:measurement_basis(waitangi_partnership_be_t1920, observed).
narrative_ontology:measurement(waitangi_partnership_be_t1960, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 1960, 0.72).
narrative_ontology:measurement_basis(waitangi_partnership_be_t1960, observed).
narrative_ontology:measurement(waitangi_partnership_be_t1975, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 1975, 0.64).
narrative_ontology:measurement_basis(waitangi_partnership_be_t1975, observed).
narrative_ontology:measurement(waitangi_partnership_be_t1987, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 1987, 0.53).
narrative_ontology:measurement_basis(waitangi_partnership_be_t1987, observed).
narrative_ontology:measurement(waitangi_partnership_be_t2000, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 2000, 0.47).
narrative_ontology:measurement_basis(waitangi_partnership_be_t2000, observed).
narrative_ontology:measurement(waitangi_partnership_be_t2012, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 2012, 0.44).
narrative_ontology:measurement_basis(waitangi_partnership_be_t2012, observed).
narrative_ontology:measurement(waitangi_partnership_be_t2025, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 2025, 0.55).
narrative_ontology:measurement_basis(waitangi_partnership_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(waitangi_partnership_su_t1840, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 1840, 0.42).
narrative_ontology:measurement_basis(waitangi_partnership_su_t1840, observed).
narrative_ontology:measurement(waitangi_partnership_su_t1880, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 1880, 0.88).
narrative_ontology:measurement_basis(waitangi_partnership_su_t1880, observed).
narrative_ontology:measurement(waitangi_partnership_su_t1920, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 1920, 0.76).
narrative_ontology:measurement_basis(waitangi_partnership_su_t1920, observed).
narrative_ontology:measurement(waitangi_partnership_su_t1960, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 1960, 0.58).
narrative_ontology:measurement_basis(waitangi_partnership_su_t1960, observed).
narrative_ontology:measurement(waitangi_partnership_su_t1975, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 1975, 0.5).
narrative_ontology:measurement_basis(waitangi_partnership_su_t1975, observed).
narrative_ontology:measurement(waitangi_partnership_su_t1987, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 1987, 0.4).
narrative_ontology:measurement_basis(waitangi_partnership_su_t1987, observed).
narrative_ontology:measurement(waitangi_partnership_su_t2000, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 2000, 0.33).
narrative_ontology:measurement_basis(waitangi_partnership_su_t2000, observed).
narrative_ontology:measurement(waitangi_partnership_su_t2012, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 2012, 0.31).
narrative_ontology:measurement_basis(waitangi_partnership_su_t2012, observed).
narrative_ontology:measurement(waitangi_partnership_su_t2025, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 2025, 0.4).
narrative_ontology:measurement_basis(waitangi_partnership_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(waitangi_sovereignty_allocation__partnership_reading, identity_coordination).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__partnership_reading, crown_sovereignty_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__partnership_reading, rangatiratanga_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__partnership_reading, foreshore_and_seabed_title).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the ε-invariance principle: the colloquial label 'the Treaty of Waitangi' covers three structurally distinct constraints (the three declared readings), each with its own ε, beneficiary/victim structure, and classification. Upstream/downstream structure: crown_sovereignty_reading is the historical official reading from which the partnership reading departed judicially (1975-1987); the partnership reading cites the Treaty's guarantees (shared upstream text) as warrant while exerting downstream pressure on the rangatiratanga_reading by converting retained-authority claims into consultation rights and full-and-final settlements. The foreshore_and_seabed_title edge records a concrete contamination event: litigation success under the partnership frame (Ngati Apa 2003) triggered legislative reversal (2004), demonstrating how this constraint's purity propagates to neighboring title constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
