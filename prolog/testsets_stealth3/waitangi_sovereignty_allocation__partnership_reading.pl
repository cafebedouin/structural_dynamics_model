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
 *   human_readable: Treaty of Waitangi Partnership Doctrine (Principles Regime)
 *   domain: constitutional/indigenous_rights/post_colonial_governance
 *
 * SUMMARY:
 *   The Treaty of Waitangi 1840 survives as two texts that diverge on the
 *   sovereignty question, and the partnership reading — instantiated in New
 *   Zealand law through the Treaty principles doctrine (Lands case 1987), the
 *   Waitangi Tribunal (1975, retrospective jurisdiction 1985), statutory
 *   incorporation in sixty-plus acts, and the settlement process running
 *   since 1989 — holds that the compact created an ongoing relationship
 *   requiring good-faith consultation and active protection of Maori
 *   interests. This story authors ONLY that reading, as a clean
 *   epsilon-invariant constraint. Its referent is the standing arrangement
 *   under contest: the principles-doctrine regime as actually operated
 *   1975–2025 (interval units: years elapsed since 1975), assessed by this
 *   reading's own lights. On those lights the arrangement genuinely delivers
 *   consultation rights, redress machinery worth over NZ$2.7 billion, and
 *   co-governance seats, while falling materially short of what the reading
 *   demands: consultation that advises rather than binds, full-and-final
 *   extinguishment of historical claims at quanta below loss values,
 *   Crown-controlled interpretation of the principles, and parliamentary
 *   supremacy sitting untouched above every obligation. KEY AGENTS (by
 *   structural relationship): - crown_executive: Agenda-setting
 *   beneficiary-payer ([institutional]/[arbitrage]) — administers
 *   consultation, defines the principles, negotiates settlements; collects
 *   legitimacy and retained discretion, pays redress - nz_parliament:
 *   Ultimate agenda-setter ([institutional]/[arbitrage]) — retains
 *   sovereignty above all Treaty obligations; can repeal the framework by
 *   simple majority - maori_iwi_and_hapu: Primary beneficiary-payer
 *   ([organized]/[identity_locked]) — collects consultation, protection
 *   duties and redress; bears advisory ceilings and accumulating unresolved
 *   claims; cannot exit - settled_iwi_entities: Beneficiary-payer
 *   ([organized]/[identity_locked]) — received assets and co-governance;
 *   traded perpetual claims for defined packages - unsettled_iwi_claimants:
 *   Payer ([organized]/[trapped]) — grievances open, benchmarks set by
 *   others' discounts, no redress path outside the channel -
 *   foreshore_seabed_customary_holders: Payer ([moderate]/[trapped]) —
 *   recognised customary title legislated away in 2004 -
 *   general_public_taxpayers: Payer-beneficiary ([moderate]/[constrained]) —
 *   funds redress, gains stability, never consulted on design -
 *   waitangi_tribunal: Institutional observer ([institutional]/[analytical])
 *   — investigates and recommends, enforces nothing -
 *   constitutional_judiciary: Observer ([institutional]/[analytical]) —
 *   authored and maintains the doctrine, dependent on statutory incorporation
 *   - maori_sovereignty_movement: Excluded voice ([organized]/[trapped]) —
 *   seeks structural transformation; has no seat in Crown-addressed machinery
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(waitangi_sovereignty_allocation__partnership_reading, 0.58).
domain_priors:suppression_score(waitangi_sovereignty_allocation__partnership_reading, 0.6).
domain_priors:theater_ratio(waitangi_sovereignty_allocation__partnership_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__partnership_reading, tangled_rope).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__partnership_reading, "Treaty of Waitangi Partnership Doctrine (Principles Regime)").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__partnership_reading, "constitutional/indigenous_rights/post_colonial_governance").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__partnership_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__partnership_reading, 'ed072c72-a9f8-437c-8c3f-3f0c74fbc1a4').
narrative_ontology:cs_kernel_codification('ed072c72-a9f8-437c-8c3f-3f0c74fbc1a4', fixed_text).
narrative_ontology:cs_authority_grounding('ed072c72-a9f8-437c-8c3f-3f0c74fbc1a4', lineage).
narrative_ontology:cs_interpretation_layer_present('ed072c72-a9f8-437c-8c3f-3f0c74fbc1a4').
narrative_ontology:cs_reading_relation('ed072c72-a9f8-437c-8c3f-3f0c74fbc1a4', waitangi_sovereignty_allocation__crown_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('ed072c72-a9f8-437c-8c3f-3f0c74fbc1a4', waitangi_sovereignty_allocation__rangatiratanga_reading, coexists_with).
narrative_ontology:cs_axiom('ed072c72-a9f8-437c-8c3f-3f0c74fbc1a4', foundational, treaty_obligations_bind_crown_conduct).
narrative_ontology:cs_axiom_status(treaty_obligations_bind_crown_conduct, holdable).
narrative_ontology:cs_axiom_grounding('ed072c72-a9f8-437c-8c3f-3f0c74fbc1a4', treaty_obligations_bind_crown_conduct, conventional).
narrative_ontology:cs_axiom('ed072c72-a9f8-437c-8c3f-3f0c74fbc1a4', foundational, active_protection_exceeds_non_interference).
narrative_ontology:cs_axiom_status(active_protection_exceeds_non_interference, holdable).
narrative_ontology:cs_axiom_grounding('ed072c72-a9f8-437c-8c3f-3f0c74fbc1a4', active_protection_exceeds_non_interference, deontological).
narrative_ontology:cs_reference_frame('ed072c72-a9f8-437c-8c3f-3f0c74fbc1a4', treaty_as_good_faith_partnership).
narrative_ontology:cs_drift_state('ed072c72-a9f8-437c-8c3f-3f0c74fbc1a4', contemporary_excision_pressure_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ed072c72-a9f8-437c-8c3f-3f0c74fbc1a4', '').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__partnership_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__partnership_reading, maori_iwi_and_hapu).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__partnership_reading, settled_iwi_entities).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__partnership_reading, crown_executive).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__partnership_reading, unsettled_iwi_claimants).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__partnership_reading, foreshore_seabed_customary_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__partnership_reading, general_public_taxpayers).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__partnership_reading, maori_iwi_and_hapu).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__partnership_reading, settled_iwi_entities).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__partnership_reading, general_public_taxpayers).
narrative_ontology:constraint_vindicates(waitangi_sovereignty_allocation__partnership_reading, treaty_principles_doctrine).
narrative_ontology:constraint_vindicates(waitangi_sovereignty_allocation__partnership_reading, active_protection_fiduciary_duty).
narrative_ontology:constraint_vindicates(waitangi_sovereignty_allocation__partnership_reading, good_faith_consultation_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ministers and departments exercise governing authority over New Zealand. Since the late 1980s they must consider Treaty principles when exercising statutory powers, consult iwi on policies affecting Maori interests, negotiate and fund settlements of historical claims, and defend the doctrine when challenged. Consultation shapes but rarely determines decisions; settlements are paid from consolidated revenue across decades. The executive also collects the framework's quietest product: a legitimacy account in which its governance is Treaty-consistent rather than simply imposed. Nothing above parliament binds it, and it can reshape the framework itself — as the 2023–2025 attempt to strip principles clauses from legislation shows.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, crown_executive, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__partnership_reading, crown_executive, beneficiary).

% Retains plenary legislative power. Every Treaty obligation operating in statute exists only because parliament enacted it and can be amended or repealed by simple majority — demonstrated by the Foreshore and Seabed Act 2004, which legislated over a recognised customary-property interest, and by the recent programme removing principles clauses. MPs answer to electorates in which Maori-descended voters are a minority, so the constraint on parliament is political rather than legal.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, nz_parliament, agenda_setter,
    institutional, biographical, arbitrage, national).

% Maori collectives hold the relationship side of the partnership. They receive consultation rights on Crown actions touching their interests, active-protection duties owed to them, standing to take historical grievances to the Waitangi Tribunal, and negotiated redress. Membership is fixed by descent and territory — leaving is not among their options — and their formal influence is advisory: recommendations may be declined, consultation may arrive after directions are set, and claims outside settled scopes accumulate without a hearing path.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, maori_iwi_and_hapu, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__partnership_reading, maori_iwi_and_hapu, payer).

% Post-settlement governance bodies such as Te Runanga o Ngai Tahu and Waikato-Tainui received negotiated asset packages, formal apologies, statutory acknowledgements, and co-governance seats on rivers, harbours and conservation land. In accepting settlement they agreed their historical claims are finally settled — trading perpetual claim-rights for defined packages — and now operate substantial commercial arms while carrying the ongoing cost of maintaining Crown relationships.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, settled_iwi_entities, beneficiary,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__partnership_reading, settled_iwi_entities, payer).

% Iwi and hapu without completed settlements — most prominently Ngapuhui, in negotiation for over fifteen years — carry unresolved grievances while watching the quanta earlier claimants accepted become the benchmark. Their claims remain open, but the machinery's momentum and the full-and-final norm press them toward packages sized well below their own loss valuations, and stepping outside the negotiation channel means forfeiting the only redress path that exists.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, unsettled_iwi_claimants, payer,
    organized, generational, trapped, regional).

% Hapu whose customary marine-title claims were recognised by the Court of Appeal in 2003 and then extinguished by the Foreshore and Seabed Act 2004, replaced with lesser statutory instruments. They experienced direct legislative override of a recognised interest — the sharpest available demonstration that consultation duties yield to parliamentary majority — and their successors now hold only what replacement statutes concede.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, foreshore_seabed_customary_holders, payer,
    moderate, generational, trapped, regional).

% Fund settlements and co-governance arrangements from consolidated revenue — over NZ$2.7 billion in historical settlements to date — and live inside resource-sharing regimes. They gain a standing dispute-resolution channel that avoids open constitutional conflict and a national story enriched by acknowledgement, but were never themselves consulted on the framework's design and experience it mainly as cost line and news cycle.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, general_public_taxpayers, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__partnership_reading, general_public_taxpayers, beneficiary).

% Permanent commission of inquiry established in 1975, empowered since 1985 to investigate Crown breaches back to 1840. Its reports find breaches and recommend remedies but bind nobody; its practical force comes from evidentiary authority and from feeding the settlement negotiations that follow its findings.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, waitangi_tribunal, observer,
    institutional, generational, analytical, national).

% The courts authored the principles doctrine in the 1987 Lands case and continue to refine it, translating two divergent treaty texts into operative duties such as partnership and active protection. They interpret but cannot supply enforcement parliament withholds; the doctrine constrains only where statutes incorporate it.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, constitutional_judiciary, observer,
    institutional, generational, analytical, national).

% Advocates of constitutional transformation, organised through initiatives such as Matike Mai Aotearoa, argue that consultation inside Crown supremacy is structurally incapable of honouring retained authority and seek parallel or genuinely shared institutions. The framework offers them no seat: it processes only claims addressed to the Crown, so they work outside it through assemblies, education and protest.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, maori_sovereignty_movement, excluded,
    organized, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(waitangi_sovereignty_allocation__partnership_reading, crown_executive).
narrative_ontology:fixing_cost_class(waitangi_sovereignty_allocation__partnership_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standing interface through which a Westminster state and Maori collectives claiming the same territory coordinate: consultation before decisions affecting Maori interests, a commission of inquiry for historical breaches, and negotiated settlement of grievances — managing deep coexistence without constitutional rupture.
% TRANSFER_FUNCTION: Moves decision-shaping influence, compensation assets, statutory acknowledgements and co-governance seats from the Crown and public estate toward Maori collectives; moves claim-finality (permanent extinguishment of historical claims), interpretive control over the principles, and governance legitimacy back toward the Crown.
% ABSENT_VOICES: Constitutional-transformation advocates who reject processing retained-authority claims exclusively through Crown-created channels; hapu outside large settled-iwi structures whose interests are mediated by post-settlement corporations they did not design; and the wider public, which funds the framework but was never consulted on it. They sit outside the settlement and consultation machinery — in protest politics, independent assemblies and academia.
% DISAPPEARANCE_RATIONALE: If the principles doctrine, the tribunal and the settlement channel vanished overnight, hundreds of lodged claims would be stranded, co-governance arrangements on rivers and conservation estates would collapse, the dispute-resolution channel that replaced the protest-and-litigation cycles of 1860–1975 would disappear, and Crown-Maori relations would revert to raw political contest — with immediate litigation surges and renewed mass protest.
% FOUNDING_PROBLEM: After roughly 130 years in which Westminster governments treated the 1840 compact as spent, accelerating land alienation and Maori protest forced a question the original texts never settled: how a sovereign parliament honours continuing obligations to the descendants of the other signatory community, and through what machinery grievances accumulating since 1840 are heard and redressed.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the Court of Appeal's 1987 Lands-case judgments describing the Treaty as the foundation of New Zealand's developing constitution; the cross-party statute book, with settlement legislation enacted by governments of both major parties for four decades; Waitangi Tribunal findings adopted by successive Cabinets of every stripe; and international review, notably UN CERD's criticism of the 2004 foreshore legislation. Each attests from a non-beneficiary seat that the underlying obligation-question remains unresolved and live.
narrative_ontology:disappearance_verdict(waitangi_sovereignty_allocation__partnership_reading, world_rearranges).
narrative_ontology:founding_problem_status(waitangi_sovereignty_allocation__partnership_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(waitangi_sovereignty_allocation__partnership_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(waitangi_sovereignty_allocation__partnership_reading, 'none', 1).
narrative_ontology:epsilon_provenance(waitangi_sovereignty_allocation__partnership_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.58 because the arrangement moves real value in both directions while carrying a substantial extractive component: open-ended historical claims convert into permanent finality at negotiated discounts, interpretation of the principles stays with the party being constrained, and the consultation ceiling is advisory. The temporal series shows that component growing as settlement machinery scaled — more finality conversions, larger extinguished-liability stock — which is accumulation of the extractive layer on top of a genuine coordination base. Suppression is authored at 0.60 as a raw structural property (unscaled by power or scope): enforcement is statutory and judicial rather than violent, but it includes direct legislative override (2004), funding leverage over claimant negotiations, and participants who cannot exit because descent and territory fix membership. The suppression series tracks enforcement-capacity history — build-up through the SOE/Lands era, the settlement-deed hardening, the 2004 peak, then erosion pressure from the excision programme — which is why it is authored as a trajectory rather than left to the static scalar. Theater is 0.46: partnership language and consultation rituals are heavily performative and increasingly post-decisional, but the tribunal produces substantive findings and settlements transfer real assets. Accessibility_collapse is 0.48 — litigation, protest and international fora persist as alternatives, though they weaken markedly once a group accepts full-and-final settlement. Resistance is 0.60: repeated protest waves (land march, Bastion Point, the 2004 hikoi, Ihumatao), Ngapuhui's long refusal to settle, and the current doctrinal counter-movement. The three series run on one shared seven-point grid so every metric is authored at every examined time point; the 2004 suppression spike is an episodic override riding a ratcheting baseline rather than a full oscillation, so no cyclical model is asserted.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the crown_executive seat the arrangement reads as an obligation-discharge machine the executive itself designed and can steer — coordination it built, with costs it budgets (rope-flavored). From the iwi seats the same structure reads as partial delivery purchased with permanent finality: real assets received, real claims extinguished, real ceiling on influence (extractive flavor). From the taxpayer seat it is a cost line attached to a stability good. From the excluded sovereignty-movement seat it is co-optation — a channel that consumes claim energy while changing nothing fundamental. The payer and beneficiary seats disagree not about facts but about which flows dominate, and the engine resolves that per-seat from power, exit and directionality rather than from this claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries map to real recipients: maori_iwi_and_hapu and settled_iwi_entities collect consultation rights, protection duties and redress (low d, subsidy end); crown_executive collects legitimacy and a managed-dispute environment. Declared victims map to real cost-bearers: unsettled_iwi_claimants and foreshore_seabed_customary_holders bear finality conversion, discounted quanta and outright override (high d, target end). One structural caveat is recorded rather than papered over: crown_executive is genuinely double-sided — declared beneficiary, yet the constraint's primary extraction target is precisely its unilateral discretion, and settlement outflows come from its estate. The automatic derivation will likely place this seat nearer the beneficiary end than its mixed reality warrants. A directionality override was considered and deliberately omitted because overrides key on the power atom, and crown_executive shares 'institutional' with nz_parliament, waitangi_tribunal and constitutional_judiciary; a blanket correction would drag analytical and observer seats toward the target end. The mixed position is documented here instead. Taxpayers sit mid-range: real outflows, diffuse stability benefit, voting leverage. Tribunal and judiciary carry analytical exits and negligible flows in either direction.
 *
 * MANDATROPHY ANALYSIS:
 *   The R5 interview blocks a mandatrophy finding: the founding problem — how a sovereign parliament honours continuing obligations to the other signatory community — is live, attested by courts, the cross-party statute book, tribunal findings adopted across governments, and international review. The constraint is not performing a dead mandate; it is performing a contested live one. The classification nevertheless prevents two opposite mislabels. Read without beneficiary/victim data, the arrangement presents as pure redress (rope) — hiding the finality conversion and interpretive capture; read as its critics present it, it presents as pure co-optation (snare) — hiding the genuine coordination that ended a century of protest-and-confiscation cycles. Tangled rope keeps both faces load-bearing. The forward risk is degradation rather than obsolescence: if the excision programme hollows out enforcement while partnership language persists, the constraint drifts piton-ward (ritual without force) — tracked through the theater series and the doctrine-durability omega rather than asserted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This story authors only the partnership_reading of the waitangi_sovereignty_allocation kernel. Which of the three competing readings of the two-text 1840 compact governs the constitutional allocation, and how would each sibling change this constraint''s structure?',
    'A constitutional transformation process (Matike Mai-style deliberation reaching institutional form), sustained judicial or statutory adoption of a rival reading, or explicit designation of one text as controlling.',
    'Adoption of crown_sovereignty_reading dissolves consultation duties into revocable political grace — the victim set empties, extraction collapses toward the beneficiary side, and this constraint ceases to bind. Adoption of rangatiratanga_reading converts advisory consultation into shared or parallel authority — the victim set shifts to Crown discretion-holders, and enforcement demands rise sharply. The partnership reading persists only while the middle position holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of a contested kernel; sibling readings redraw the beneficiary/victim geometry entirely.').

omega_variable(
    article_two_textual_primacy,
    'Which text controls the meaning of rangatiratanga in Article II — the English text''s cession-qualified reading or the Maori text''s unqualified-retention reading — given that the partnership reading proceeds without resolving the divergence?',
    'Doctrinal or statutory designation of a controlling text, or a settled interpretive synthesis accepted by both parties to the relationship.',
    'English-text primacy pulls the standing arrangement toward the crown_sovereignty sibling and softens the obligations; Maori-text primacy pulls toward the rangatiratanga sibling and hardens them into authority-sharing. The partnership reading survives only as the bracket over this unresolved divergence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_two_textual_primacy, conceptual, 'The two-text divergence is the located disagreement the partnership reading manages procedurally rather than settles.').

omega_variable(
    settlement_quantum_fairness,
    'Do settlement quanta represent a fair proportion of the losses they permanently extinguish — for example NZ$170 million against the 1.2 million acres confiscated from Waikato-Tainui — or does full-and-final finality convert unresolved liability into permanent Crown release at a discount?',
    'Independent economic valuation of confiscation, coercive purchase and administrative losses, appropriately compounded, compared against full settlement packages including relativity mechanisms.',
    'Gross undervaluation raises the extractive component and drifts the arrangement snare-ward; approximate fairness lowers epsilon toward the rope boundary and strengthens the reading''s own redress narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settlement_quantum_fairness, empirical, 'Whether redress quanta track the magnitude of the losses they extinguish forever.').

omega_variable(
    consultation_outcome_materiality,
    'What proportion of Crown consultations with iwi materially alter the decision consulted upon, versus occurring after the direction is already set?',
    'Audit of Cabinet papers, regulatory impact statements and settlement-negotiation records, tracking outcome-change rates attributable to consultation input.',
    'A high material-alteration rate means the authored theater_ratio is overstated and the coordination component stronger than measured; a low rate confirms ritual consultation as the face the arrangement presents while extraction continues beneath it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consultation_outcome_materiality, empirical, 'Materiality of consultation outcomes versus performative compliance with the duty to consult.').

omega_variable(
    doctrine_durability_under_excision,
    'Can the principles doctrine durably constrain a determined parliamentary majority, given the 2023–2025 programme to remove principles clauses from legislation and the defeat of the Treaty Principles Bill amid record select-committee participation?',
    'Track statutory practice over the coming decade: survival or removal of principles clauses, the design of replacement instruments, and litigation outcomes testing the doctrine''s residue.',
    'If excision succeeds broadly, enforcement decays while partnership language persists and the arrangement drifts piton-ward — performance maintained without force. If the programme fails and doctrine consolidates, enforcement stabilises near its 2000s peak and the tangled-rope profile holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_durability_under_excision, empirical, 'Whether the enforcement infrastructure holds, decays, or hardens under current political pressure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(waitangi_sovereignty_allocation__partnership_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wait_tr_t0, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(wait_tr_t0, observed).
narrative_ontology:measurement(wait_tr_t10, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 10, 0.26).
narrative_ontology:measurement_basis(wait_tr_t10, observed).
narrative_ontology:measurement(wait_tr_t18, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 18, 0.33).
narrative_ontology:measurement_basis(wait_tr_t18, observed).
narrative_ontology:measurement(wait_tr_t29, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 29, 0.41).
narrative_ontology:measurement_basis(wait_tr_t29, observed).
narrative_ontology:measurement(wait_tr_t38, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 38, 0.44).
narrative_ontology:measurement_basis(wait_tr_t38, observed).
narrative_ontology:measurement(wait_tr_t44, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 44, 0.45).
narrative_ontology:measurement_basis(wait_tr_t44, observed).
narrative_ontology:measurement(wait_tr_t50, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 50, 0.46).
narrative_ontology:measurement_basis(wait_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(wait_be_t0, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(wait_be_t0, observed).
narrative_ontology:measurement(wait_be_t10, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement_basis(wait_be_t10, observed).
narrative_ontology:measurement(wait_be_t18, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 18, 0.47).
narrative_ontology:measurement_basis(wait_be_t18, observed).
narrative_ontology:measurement(wait_be_t29, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 29, 0.53).
narrative_ontology:measurement_basis(wait_be_t29, observed).
narrative_ontology:measurement(wait_be_t38, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 38, 0.55).
narrative_ontology:measurement_basis(wait_be_t38, observed).
narrative_ontology:measurement(wait_be_t44, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 44, 0.57).
narrative_ontology:measurement_basis(wait_be_t44, observed).
narrative_ontology:measurement(wait_be_t50, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 50, 0.58).
narrative_ontology:measurement_basis(wait_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(wait_su_t0, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(wait_su_t0, observed).
narrative_ontology:measurement(wait_su_t10, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement_basis(wait_su_t10, observed).
narrative_ontology:measurement(wait_su_t18, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 18, 0.58).
narrative_ontology:measurement_basis(wait_su_t18, observed).
narrative_ontology:measurement(wait_su_t29, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 29, 0.72).
narrative_ontology:measurement_basis(wait_su_t29, observed).
narrative_ontology:measurement(wait_su_t38, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 38, 0.68).
narrative_ontology:measurement_basis(wait_su_t38, observed).
narrative_ontology:measurement(wait_su_t44, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 44, 0.66).
narrative_ontology:measurement_basis(wait_su_t44, observed).
narrative_ontology:measurement(wait_su_t50, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 50, 0.6).
narrative_ontology:measurement_basis(wait_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(waitangi_sovereignty_allocation__partnership_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__partnership_reading, waitangi_sovereignty_allocation__crown_sovereignty_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__partnership_reading, waitangi_sovereignty_allocation__rangatiratanga_reading).

% DUAL FORMULATION NOTE:
% Colloquially 'the Treaty of Waitangi' names one constitutional settlement; it is three structurally distinct constraints, one per reading of the two-text kernel. This file instantiates the partnership reading — the operational middle: obligations bind, but only as far as statute and convention carry them. The crown_sovereignty reading is the upstream, historically dominant claim (obligations as grace beneath Westminster supremacy); the rangatiratanga reading is the downstream, authority-maximal claim (retained tino rangatiratanga, kawanatanga limited to settlers). Each carries its own epsilon, beneficiaries and victims, so each is a separate story. Logical foreclosure in this kernel lives on the crown_sovereignty x rangatiratanga edge — complete cession directly contradicts retained full authority — and belongs to those files; the partnership reading stands in genuine coexistence with each sibling, which is precisely why it has persisted for fifty years as the compromise the machinery actually runs on.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
