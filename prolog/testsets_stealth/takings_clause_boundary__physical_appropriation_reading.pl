% ============================================================================
% CONSTRAINT STORY: takings_clause_boundary__physical_appropriation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_takings_clause_boundary__physical_appropriation_reading, []).

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
 *   constraint_id: takings_clause_boundary__physical_appropriation_reading
 *   human_readable: Takings Clause Physical Appropriation Boundary
 *   domain: constitutional/legal/political
 *
 * SUMMARY:
 *   The Fifth Amendment's Compensation Clause fixes a boundary between
 *   government actions that require payment to the owner and actions that do
 *   not. The physical appropriation reading draws that boundary narrowly:
 *   only direct physical seizure or permanent physical occupation counts as a
 *   taking triggering just compensation; regulations that diminish or destroy
 *   economic value trigger nothing. This story instantiates that one reading
 *   as a clean, epsilon-invariant constraint. It belongs to a constraint
 *   family decomposing the takings_clause_boundary kernel: the
 *   regulatory_takings_reading (compensation when regulation goes 'too far')
 *   carries a far larger victim set and higher epsilon; the
 *   categorical_takings_reading (per se treatment of physical occupations and
 *   total value elimination, factor-balancing otherwise) sits between them.
 *   Those siblings are separate files with their own beneficiaries, victims,
 *   and classifications; this file authors only the physical reading's
 *   structure. The claim/metric gap is deliberate: the reading is CLAIMED
 *   here as tangled_rope — a genuine bright-line coordination service wrapped
 *   around a systematic uncompensated transfer — while the authored metrics
 *   independently describe materially extractive, actively enforced operation
 *   that has intensified as the regulatory state grew. The engine computes
 *   per-seat classifications from the structural data; the authored claim
 *   does not adjudicate them.
 *
 * KEY AGENTS:
 *   - - land_use_regulators: Primary beneficiary and agenda-setter (institutional/arbitrage) — obtains compliance and redirected parcel value without payment; substitutes instruments when one draws challenge
 *   - - federal_takings_courts: Boundary administrator (institutional/constrained) — maintains the physical-appropriation line and dismisses claims outside it
 *   - - regulated_property_owners: Primary target (moderate/constrained) — bears uncompensated regulatory losses; land immobile, restriction runs with title
 *   - - development_rights_holders: Secondary target (powerful/arbitrage) — sophisticated investors who price and diversify regulatory risk where ordinary owners cannot
 *   - - condemned_property_owners: Protected beneficiary (moderate/constrained) — sit inside the guaranteed-payment zone for physical seizure and occupation
 *   - - general_taxpayer_base: Diffuse beneficiary (powerless/mobile) — enjoys regulatory benefits without paying owners for redirected value
 *   - - small_parcel_owner_advocates: Excluded voice (organized/trapped) — presses for broader compensation through legislation and initiatives with no seat in doctrinal formation
 *   - - constitutional_scholars: Analytical observer (analytical/analytical) — maps the doctrine's evolution and quantifies loss incidence without deciding anything
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(takings_clause_boundary__physical_appropriation_reading, 0.61).
domain_priors:suppression_score(takings_clause_boundary__physical_appropriation_reading, 0.51).
domain_priors:theater_ratio(takings_clause_boundary__physical_appropriation_reading, 0.29).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 0.51).
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, theater_ratio, 0.29).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(takings_clause_boundary__physical_appropriation_reading, tangled_rope).
narrative_ontology:human_readable(takings_clause_boundary__physical_appropriation_reading, "Takings Clause Physical Appropriation Boundary").
narrative_ontology:topic_domain(takings_clause_boundary__physical_appropriation_reading, "constitutional/legal/political").

domain_priors:requires_active_enforcement(takings_clause_boundary__physical_appropriation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(takings_clause_boundary__physical_appropriation_reading, '36bff069-6a3b-4b2d-829e-a2860831199d').
narrative_ontology:cs_kernel_codification('36bff069-6a3b-4b2d-829e-a2860831199d', fixed_text).
narrative_ontology:cs_authority_grounding('36bff069-6a3b-4b2d-829e-a2860831199d', lineage).
narrative_ontology:cs_interpretation_layer_present('36bff069-6a3b-4b2d-829e-a2860831199d').
narrative_ontology:cs_reading_relation('36bff069-6a3b-4b2d-829e-a2860831199d', takings_clause_boundary__regulatory_takings_reading, coexists_with).
narrative_ontology:cs_reading_relation('36bff069-6a3b-4b2d-829e-a2860831199d', takings_clause_boundary__categorical_takings_reading, influences).
narrative_ontology:cs_axiom('36bff069-6a3b-4b2d-829e-a2860831199d', foundational, takings_clause_reaches_only_appropriations).
narrative_ontology:cs_axiom_status(takings_clause_reaches_only_appropriations, holdable).
narrative_ontology:cs_axiom_grounding('36bff069-6a3b-4b2d-829e-a2860831199d', takings_clause_reaches_only_appropriations, conventional).
narrative_ontology:cs_axiom('36bff069-6a3b-4b2d-829e-a2860831199d', foundational, regulatory_losses_are_background_ownership_risk).
narrative_ontology:cs_axiom_status(regulatory_losses_are_background_ownership_risk, holdable).
narrative_ontology:cs_axiom_grounding('36bff069-6a3b-4b2d-829e-a2860831199d', regulatory_losses_are_background_ownership_risk, deontological).
narrative_ontology:cs_reference_frame('36bff069-6a3b-4b2d-829e-a2860831199d', founding_era_appropriation_guarantee).
narrative_ontology:cs_drift_state('36bff069-6a3b-4b2d-829e-a2860831199d', contemporary_regulatory_state, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('36bff069-6a3b-4b2d-829e-a2860831199d', '').
narrative_ontology:cs_kernel_id(takings_clause_boundary__physical_appropriation_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__physical_appropriation_reading, land_use_regulators).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__physical_appropriation_reading, general_taxpayer_base).
narrative_ontology:constraint_victim(takings_clause_boundary__physical_appropriation_reading, regulated_property_owners).
narrative_ontology:constraint_victim(takings_clause_boundary__physical_appropriation_reading, development_rights_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__physical_appropriation_reading, development_rights_holders).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__physical_appropriation_reading, condemned_property_owners).
narrative_ontology:constraint_vindicates(takings_clause_boundary__physical_appropriation_reading, police_power_doctrine).
narrative_ontology:constraint_vindicates(takings_clause_boundary__physical_appropriation_reading, narrow_textualist_takings_construction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Municipal planning boards, state agencies, and federal land managers impose zoning, environmental, and use restrictions on private parcels. Because payment is owed only when property is physically seized or permanently occupied, they may deploy regulations that eliminate most of a parcel's economic value without budgetary consequence. When one regulatory instrument draws legal challenge, they substitute another instrument with similar effect. Their regulatory output is planned around this cost structure remaining in place.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, land_use_regulators, agenda_setter,
    institutional, generational, arbitrage, national).

% Federal judges, culminating in the Supreme Court, decide which government actions count as takings requiring payment. They maintain the line that physical seizure and permanent occupation trigger compensation while value-diminishing regulation generally does not, dismissing or rejecting claims outside that line. They cannot decline the question when properly presented cases arrive, and they operate within accumulated precedent spanning a century of regulatory-growth disputes.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, federal_takings_courts, agenda_setter,
    institutional, civilizational, constrained, national).

% Owners whose parcels are subjected to use restrictions, permitting regimes, or conservation requirements that reduce value or block intended uses. They receive nothing unless the government physically occupies or seizes the property. Their options are complying, selling at the regulated value, suing under doctrines that rarely succeed, or petitioning legislatures for discretionary relief. Land cannot be moved, and the restriction runs with the title to any buyer.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, regulated_property_owners, payer,
    moderate, biographical, constrained, national).

% Investors and developers who acquire land expecting to build, subdivide, or extract resources. Sophisticated operators price regulatory risk before purchase, stage projects to preserve options, diversify across jurisdictions, and negotiate entitlements — tools largely unavailable to ordinary owners. When regulation lands anyway, the loss is theirs to absorb, but their positioning lets them avoid the worst of it and sometimes profit from the entitlement process itself.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, development_rights_holders, payer,
    powerful, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(takings_clause_boundary__physical_appropriation_reading, development_rights_holders, beneficiary).

% Owners whose property is formally condemned or physically occupied by government action. They sit inside the guaranteed-payment zone: the public must pay for what it takes. Their exposure is confined to disputes over how much the payment is worth, not over whether payment is owed at all.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, condemned_property_owners, beneficiary,
    moderate, biographical, constrained, national).

% The diffuse public that enjoys the benefits of land-use regulation — open space, environmental protection, orderly development — without paying owners for the value those measures redirect. Individually each person's stake is invisible; collectively they would face higher taxes or reduced services if regulatory losses were compensable. Any individual can move to another jurisdiction, but the same allocation awaits nearly everywhere.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, general_taxpayer_base, beneficiary,
    powerless, generational, mobile, national).

% Property-rights organizations and owner associations that press for broader compensation duties through legislation, ballot initiatives, and amicus participation. They have no seat in the doctrinal conversation that fixes the compensation boundary — that line is drawn in judicial opinions and agency practice — and their legislative wins are partial, reversible, and jurisdictionally scattered. Their only forums are ones whose outcomes the courts can override.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, small_parcel_owner_advocates, excluded,
    organized, biographical, trapped, national).

% Legal historians, economists, and theorists who map the doctrine's evolution, test its coherence against founding-era materials, and quantify who bears regulatory losses. They observe, publish, and testify, but decide nothing; their analyses enter the system only when litigants or judges choose to cite them.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(takings_clause_boundary__physical_appropriation_reading, land_use_regulators).
narrative_ontology:fixing_cost_class(takings_clause_boundary__physical_appropriation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes a single administrable test for when government must pay for effects on property: physical seizure or permanent occupation triggers payment; lesser regulatory burdens do not. Governments gain predictable freedom to regulate; owners gain a guaranteed payment floor against outright confiscation; courts dispose of claims with a threshold question instead of open-ended valuation of every regulatory impact.
% TRANSFER_FUNCTION: Moves the economic losses of land-use regulation from the general public onto the specific owners whose parcels are restricted; and moves public funds to owners whenever government physically takes or permanently occupies property.
% ABSENT_VOICES: Owners appear only as individual litigants after a loss has crystallized; no seat represents them as a class when the boundary is drawn. Community beneficiaries of regulation are equally absent — they receive the redirected value silently and never defend the allocation they enjoy. Future owners inherit the risk allocation without ever having been represented in its creation.
% DISAPPEARANCE_RATIONALE: If the boundary vanished overnight, every land-use restriction would carry potential compensation liability. Governments would immediately face mass claims: they would pay, deregulate, or pivot to instruments outside the compensation net, and land markets would reprice around restored development expectations. The physical-seizure guarantee would likely survive in some form, but the regulatory cost structure that thousands of jurisdictions currently plan around would collapse and have to be rebuilt deliberately.
% FOUNDING_PROBLEM: Protecting holders of private property from government confiscation without payment. The founding generation's experience with quartering soldiers, wartime seizures, and forfeiture made uncompensated taking a paradigm grievance; the Clause was written so that the public pays when it takes private property for public use.
% FOUNDING_PROBLEM_CORROBORATION: Founding-era records and early state constitutional texts corroborate the anti-seizure core of the problem, and nineteenth-century treatise writers described the guarantee in appropriation terms. Whether the founding problem extended to regulatory deprivations is disputed by legal historians aligned with competing readings, and no source outside the parties settles it — the corroboration covers the physical core, not the regulatory extension, and this file does not claim otherwise.
narrative_ontology:disappearance_verdict(takings_clause_boundary__physical_appropriation_reading, world_rearranges).
narrative_ontology:founding_problem_status(takings_clause_boundary__physical_appropriation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(takings_clause_boundary__physical_appropriation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(takings_clause_boundary__physical_appropriation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(takings_clause_boundary__physical_appropriation_reading, 0.61, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(takings_clause_boundary__physical_appropriation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(takings_clause_boundary__physical_appropriation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(takings_clause_boundary__physical_appropriation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (epsilon 0.61 at interval end) is substantial because the boundary systematically leaves a large and growing class of losses uncompensated: as the regulatory state expanded across the interval, ever more parcel value was redirected to public purposes without payment, and the government internalizes none of the cost of its regulatory choices. The rising series tracks that growth — at the founding-era baseline the rule mostly protected owners against seizure and extracted little; by the modern era the uncompensated-loss channel dominates. Suppression (0.51) is structural, not internalized: courts must actively reject compensation claims outside the physical category, and the enforcement effort grew as claims multiplied after the regulatory-deprivation lineage opened. Theater (0.29) rises modestly — the physical-seizure guarantee performs real work in condemnation cases, but an increasing share of the clause's public invocation is rhetorical property-protection covering a narrowing protective scope. Accessibility collapse is moderate (0.45): owners retain partial alternatives — legislative relief, ballot initiatives, sale at regulated value, due-process challenges — but none reliably reaches the blocked value. Resistance is elevated (0.55): sustained litigation, academic critique, and state-level compensation statutes contest the boundary continuously. All three tracked series run on one shared seven-point grid so every metric is authored at every examined time point; the enforcement picture is dynamic (rising suppression), so suppression_requirement is tracked rather than left to the scalar alone.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the land_use_regulators' position the arrangement enables legitimate democratic governance — zoning, conservation, and planning would be fiscally impossible if every value effect were compensable, so the boundary reads as the condition of the regulatory state itself. From the regulated_property_owners' position the same structure operates as confiscation by instrument choice: the government achieves outcomes indistinguishable from seizure while paying nothing, and the owner eats the difference. The condemned_property_owners' seat experiences the clause as a kept promise — certain payment for certain takings. The federal_takings_courts' seat sees administrability: a threshold question that keeps compensation law decidable. The engine derives these divergent classifications from power, exit, and directional position; this commentary explains why they diverge, not which is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Land_use_regulators sit nearest the beneficiary pole: the constraint subsidizes them with costless regulatory authority, and their arbitrage-grade instrument substitution further damps any cost they bear. General_taxpayer_base is also a low-directionality beneficiary, though diffuse and unorganized. Regulated_property_owners sit near the full-target pole: they bear the transfer, their exit is constrained (immobile land, restrictions running with title), and identity of place deepens the lock for many. Development_rights_holders are targets whose directionality is pulled back toward the middle by arbitrage-grade exit — they can price, stage, and diversify regulatory risk, so the constraint extracts less completely from them than from ordinary owners despite identical formal exposure. Condemned_property_owners are beneficiaries inside the guaranteed zone. Federal_takings_courts are neither structural beneficiaries nor victims; their directionality falls to the canonical fallback near symmetry, reflecting an administrative rather than extractive or subsidized relationship.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification blocks two symmetrical misreadings. Reading the boundary as pure extraction (snare) erases the genuine coordination service: a bright, administrable line solves a real collective-action problem — governments can plan, owners hold a guaranteed payment floor against confiscation, and courts dispose of claims with a threshold question instead of open-ended valuation of every regulatory impact. Reading it as pure coordination (rope) erases the extraction: the same line that guarantees payment for physical seizure channels the entire cost of the regulatory state onto a dispersed, politically weak class of owners, and the fiscal-illusion channel (see omega fiscal_illusion_magnitude) plausibly inflates regulatory output because its costs are invisible in budgets. The founding problem — uncompensated confiscation — is contested rather than dead: the physical core remains live and honored, while the question the reading answers negatively (do regulatory deprivations count?) is precisely what the sibling readings answer differently. Mandatrophy resolution therefore turns on the victim-set boundary, not on obsolescence of the underlying problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'This constraint is one reading of the takings_clause_boundary kernel: does the Clause''s original public meaning confine compensation to physical appropriations, or does it extend to regulatory deprivations that destroy value?',
    'Founding-era usage corpora, early state constitutional takings provisions, and nineteenth-century treatise and case surveys, analyzed adversarially by historians committed to competing readings.',
    'If the regulatory reading better captures original meaning, this constraint''s victim set expands from physical-dispossession losers to all severely burdened owners and its effective extraction rises sharply; if the physical reading holds, the current narrow victim set stands and the sibling readings are later innovations rather than recoveries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Which reading of the takings kernel the founding-era meaning actually supports.').

omega_variable(
    stare_decisis_vs_original_meaning_persistence,
    'Does the physical reading persist because it captures the Clause''s meaning, or because stare decisis and institutional inertia sustain it against a century of contrary doctrinal practice?',
    'Track whether open judicial calls to reconsider the regulatory-deprivation lineage attract majorities as court composition changes, and whether scholarly originalist critique converts into doctrinal movement or remains rhetorical.',
    'Inertia-sustained persistence would indicate the boundary survives by maintenance rather than conviction, shifting the classification toward degraded inertial operation within the doctrine; meaning-grounded persistence supports a stable, functional boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stare_decisis_vs_original_meaning_persistence, empirical, 'Whether the reading''s persistence reflects conviction or institutional momentum.').

omega_variable(
    fiscal_illusion_magnitude,
    'How much does exemption from compensation liability inflate the quantity and severity of land-use regulation governments actually impose?',
    'Natural experiment comparing regulatory stringency and parcel-value outcomes in jurisdictions with voter-approved compensation statutes (the Oregon Measure 37 and Measure 49 sequence) against matched jurisdictions without them.',
    'A large behavioral response confirms the uncompensated-loss channel as operative extraction built into the boundary; a negligible response supports treating the allocation as benign background law and lowers the extraction assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_illusion_magnitude, empirical, 'Size of the regulatory-expansion effect created by costless regulatory authority.').

omega_variable(
    background_risk_framing,
    'Is allocating regulatory losses to parcel owners a neutral specification of what ownership entails, or a transfer disguised as background law?',
    'Comparative doctrinal analysis: how parallel legal systems (civil-law expropriation codes, German constructive-taking doctrine, Canadian de facto taking rules) allocate economically equivalent regulatory losses.',
    'If comparable systems compensate losses this reading absorbs, the background-risk framing loses its neutrality defense and the extraction assessment rises; if they similarly absorb such losses, the allocation looks conventional rather than extractive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(background_risk_framing, conceptual, 'Whether the owner-borne loss allocation is constitutive of ownership or a concealed transfer.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__physical_appropriation_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taki_tr_t0, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(taki_tr_t0, observed).
narrative_ontology:measurement(taki_tr_t5, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement_basis(taki_tr_t5, observed).
narrative_ontology:measurement(taki_tr_t10, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement_basis(taki_tr_t10, observed).
narrative_ontology:measurement(taki_tr_t15, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement_basis(taki_tr_t15, observed).
narrative_ontology:measurement(taki_tr_t20, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement_basis(taki_tr_t20, observed).
narrative_ontology:measurement(taki_tr_t25, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 25, 0.26).
narrative_ontology:measurement_basis(taki_tr_t25, observed).
narrative_ontology:measurement(taki_tr_t30, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 30, 0.29).
narrative_ontology:measurement_basis(taki_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(taki_be_t0, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(taki_be_t0, observed).
narrative_ontology:measurement(taki_be_t5, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 5, 0.28).
narrative_ontology:measurement_basis(taki_be_t5, observed).
narrative_ontology:measurement(taki_be_t10, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 10, 0.36).
narrative_ontology:measurement_basis(taki_be_t10, observed).
narrative_ontology:measurement(taki_be_t15, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 15, 0.44).
narrative_ontology:measurement_basis(taki_be_t15, observed).
narrative_ontology:measurement(taki_be_t20, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement_basis(taki_be_t20, observed).
narrative_ontology:measurement(taki_be_t25, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 25, 0.56).
narrative_ontology:measurement_basis(taki_be_t25, observed).
narrative_ontology:measurement(taki_be_t30, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 30, 0.61).
narrative_ontology:measurement_basis(taki_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(taki_su_t0, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(taki_su_t0, observed).
narrative_ontology:measurement(taki_su_t5, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 5, 0.27).
narrative_ontology:measurement_basis(taki_su_t5, observed).
narrative_ontology:measurement(taki_su_t10, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 10, 0.33).
narrative_ontology:measurement_basis(taki_su_t10, observed).
narrative_ontology:measurement(taki_su_t15, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 15, 0.38).
narrative_ontology:measurement_basis(taki_su_t15, observed).
narrative_ontology:measurement(taki_su_t20, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 20, 0.43).
narrative_ontology:measurement_basis(taki_su_t20, observed).
narrative_ontology:measurement(taki_su_t25, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 25, 0.47).
narrative_ontology:measurement_basis(taki_su_t25, observed).
narrative_ontology:measurement(taki_su_t30, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 30, 0.51).
narrative_ontology:measurement_basis(taki_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(takings_clause_boundary__physical_appropriation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(takings_clause_boundary__physical_appropriation_reading, regulatory_takings_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__physical_appropriation_reading, categorical_takings_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the Takings Clause' conflates three structurally distinct claims about when compensation is owed. This story (physical_appropriation_reading) is the narrowest: victim set limited to physical-dispossession losers, epsilon moderate and rising with regulatory growth. The regulatory_takings_reading sibling carries a far larger victim set (all severely burdened owners) and correspondingly higher epsilon. The categorical_takings_reading sibling hybridizes: it absorbs this reading's physical-occupation premise as its first per se pillar and adds total-value elimination, placing its epsilon between the other two. Upstream/downstream structure: this reading is upstream of the categorical reading, whose physical-occupation pillar is this reading incorporated; it coexists with the regulatory reading as competing live positions held by different judicial coalitions. Each member links the others via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
