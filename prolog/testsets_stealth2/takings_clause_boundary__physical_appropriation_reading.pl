% ============================================================================
% CONSTRAINT STORY: takings_clause_boundary__physical_appropriation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   human_readable: Takings Clause Compensation Boundary — Physical Appropriation Reading
 *   domain: constitutional_law/property_rights/regulatory_theory
 *
 * SUMMARY:
 *   The Fifth Amendment promises compensation when private property is
 *   'taken' for public use; the physical appropriation reading holds that
 *   this promise is exhausted by direct physical seizure or permanent
 *   physical occupation, and that regulation — however severe its effect on
 *   value — imposes no payment duty. The arrangement this reading sustains
 *   has a genuine coordination face: a bright line lets land-use governance
 *   proceed without a fiscal gate on every restriction while guaranteeing
 *   payment for outright dispossession. It also has an asymmetric face: the
 *   costs of public-purpose land controls concentrate on the regulated
 *   owners, with the sharpest losses falling on the small class whose
 *   holdings are rendered worthless and who receive nothing. KEY AGENTS (by
 *   structural relationship): federal_state_local_governments — primary
 *   beneficiary (institutional/arbitrage), collects regulatory freedom and
 *   fiscal savings, pays only on physical acquisition;
 *   land_use_regulatory_agencies — secondary beneficiary
 *   (institutional/constrained), run programs premised on unpaid restriction;
 *   uncompensated_regulated_property_owners — broad target class
 *   (moderate/trapped), absorb partial value losses without payment;
 *   total_value_elimination_owners — sharpest target class
 *   (powerless/trapped), wiped-out holdings receive nothing;
 *   federal_judiciary — agenda-setter (institutional/identity_locked), draws
 *   and enforces the line; property_rights_bar_and_advocates — excluded
 *   claimants (organized/mobile), press broader readings outside the
 *   administered line; legal_academia — analytical observer
 *   (analytical/analytical), sees the full structure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(takings_clause_boundary__physical_appropriation_reading, 0.48).
domain_priors:suppression_score(takings_clause_boundary__physical_appropriation_reading, 0.58).
domain_priors:theater_ratio(takings_clause_boundary__physical_appropriation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(takings_clause_boundary__physical_appropriation_reading, tangled_rope).
narrative_ontology:human_readable(takings_clause_boundary__physical_appropriation_reading, "Takings Clause Compensation Boundary — Physical Appropriation Reading").
narrative_ontology:topic_domain(takings_clause_boundary__physical_appropriation_reading, "constitutional_law/property_rights/regulatory_theory").

domain_priors:requires_active_enforcement(takings_clause_boundary__physical_appropriation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(takings_clause_boundary__physical_appropriation_reading, '84263c9c-612b-4d18-8490-bbc17b38da7e').
narrative_ontology:cs_kernel_codification('84263c9c-612b-4d18-8490-bbc17b38da7e', fixed_text).
narrative_ontology:cs_authority_grounding('84263c9c-612b-4d18-8490-bbc17b38da7e', lineage).
narrative_ontology:cs_interpretation_layer_present('84263c9c-612b-4d18-8490-bbc17b38da7e').
narrative_ontology:cs_reading_relation('84263c9c-612b-4d18-8490-bbc17b38da7e', takings_clause_boundary__regulatory_takings_reading, forecloses).
narrative_ontology:cs_reading_relation('84263c9c-612b-4d18-8490-bbc17b38da7e', takings_clause_boundary__categorical_takings_reading, forecloses).
narrative_ontology:cs_axiom('84263c9c-612b-4d18-8490-bbc17b38da7e', foundational, compensation_exhausted_by_physical_appropriation).
narrative_ontology:cs_axiom_status(compensation_exhausted_by_physical_appropriation, holdable).
narrative_ontology:cs_axiom_grounding('84263c9c-612b-4d18-8490-bbc17b38da7e', compensation_exhausted_by_physical_appropriation, conventional).
narrative_ontology:cs_axiom('84263c9c-612b-4d18-8490-bbc17b38da7e', secondary, regulatory_value_losses_are_incidents_of_ownership).
narrative_ontology:cs_axiom_status(regulatory_value_losses_are_incidents_of_ownership, holdable).
narrative_ontology:cs_axiom_grounding('84263c9c-612b-4d18-8490-bbc17b38da7e', regulatory_value_losses_are_incidents_of_ownership, deontological).
narrative_ontology:cs_reference_frame('84263c9c-612b-4d18-8490-bbc17b38da7e', ratification_era_physical_expropriation_baseline).
narrative_ontology:cs_drift_state('84263c9c-612b-4d18-8490-bbc17b38da7e', contemporary_regulatory_state_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('84263c9c-612b-4d18-8490-bbc17b38da7e', '').
narrative_ontology:cs_kernel_id(takings_clause_boundary__physical_appropriation_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__physical_appropriation_reading, federal_state_local_governments).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__physical_appropriation_reading, land_use_regulatory_agencies).
narrative_ontology:constraint_victim(takings_clause_boundary__physical_appropriation_reading, uncompensated_regulated_property_owners).
narrative_ontology:constraint_victim(takings_clause_boundary__physical_appropriation_reading, total_value_elimination_owners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(takings_clause_boundary__physical_appropriation_reading, federal_state_local_governments).
narrative_ontology:constraint_vindicates(takings_clause_boundary__physical_appropriation_reading, original_public_meaning_fixation_thesis).
narrative_ontology:constraint_vindicates(takings_clause_boundary__physical_appropriation_reading, police_power_noncompensation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Governments at every level acquire land and occupy property for roads, buildings, and public works, and pay fair market value when they do. Everything short of physical acquisition — zoning, environmental limits, historic designation, rent stabilization — they may impose without opening a payment side-account, which keeps the fiscal price of governing land near zero. When a compensation demand does arrive, they can usually reach the same goal through taxation, spending, or mandates on non-property interests, so the boundary rarely forces a choice between paying and stopping.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, federal_state_local_governments, beneficiary,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(takings_clause_boundary__physical_appropriation_reading, federal_state_local_governments, payer).

% Municipal planning boards, zoning commissions, and environmental permitting offices run programs whose entire operating design assumes that restricting use triggers no payment. Budgets and staffing are sized to a world where the primary tool of land governance is free; attaching a compensation obligation to routine permit denials would force them to shrink programs or convert them into fee-based services. They defend the boundary in litigation because their program architecture depends on it.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, land_use_regulatory_agencies, beneficiary,
    institutional, generational, constrained, local).

% Owners whose land loses part of its value to use restrictions — setback rules, wetlands limits, density caps, preservation overlays — absorb the loss with no payment. Selling does not escape the loss because the restriction is priced into the sale; relocating leaves the impaired investment behind. Their realistic channels are petitioning for variances, waiting for political turnover, or carrying the cost.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, uncompensated_regulated_property_owners, payer,
    moderate, biographical, trapped, national).

% The narrow class of owners whose holdings are rendered worthless or nearly worthless by regulation — a parcel barred from any buildable use, a mineral interest voided by a mining ban. Because no government agent physically entered or occupied the property, their injury receives nothing under the administered line. Litigation is their only channel and the doctrine closes it at the threshold; they carry the sharpest version of the loss the boundary allocates.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, total_value_elimination_owners, payer,
    powerless, biographical, trapped, national).

% The courts decide which injuries count as compensable and turn away the rest, drawing and redrawing the line case by case. Their authority rests on continuity with the ratified text and with their own precedents; abandoning the physical-appropriation line would require repudiating a century of doctrine and the interpretive commitments bound up with it. Individual judges rotate off, but the institution's self-conception as guardian of the founders' settlement travels with the office.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, federal_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).

% Litigators, trade associations, and think tanks argue that compensation should reach regulatory wipeouts, file test cases, and lobby legislatures. The operative federal rule turns their core claim away, so their effort redirects to state constitutions, ballot initiatives, and adjacent doctrines such as exactions and development-impact fees. They remain outside the line the courts administer, though not outside the courtroom.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, property_rights_bar_and_advocates, excluded,
    organized, biographical, mobile, national).

% Scholars reconstruct what 'taken' meant at ratification, model the distributive effects of alternative boundaries, and publish competing histories of the doctrine. They collect no payments and bear no losses; theirs is the seat from which the text, the administered line, and the flow of costs are visible at once.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, legal_academia, observer,
    analytical, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(takings_clause_boundary__physical_appropriation_reading, federal_state_local_governments).
narrative_ontology:fixing_cost_class(takings_clause_boundary__physical_appropriation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Draws a single bright line around the compensation obligation: government pays when it physically seizes or permanently occupies property, and otherwise governs land through regulation without a payment side-account. This lets land-use governance proceed at scale without a fiscal gate on every restriction, while preserving a guaranteed payment channel for outright dispossession.
% TRANSFER_FUNCTION: Moves the costs of public-purpose land controls — diminished value, forgone uses, extinguished development rights — from the general public onto the specific owners regulated. Cash moves the opposite direction only for physical acquisitions: treasury to owner at fair market value.
% ABSENT_VOICES: Owners suffering severe regulatory losses, above all the total-wipeout class, would object that the line excludes their injuries while socializing everyone else's gains; they speak from dismissed dockets, state-court petitions, and legislative testimony rather than from inside the operative rule. Dissenting justices and a large scholarly literature press the same objection without moving the administered line.
% DISAPPEARANCE_RATIONALE: If the boundary vanished overnight, either every value-diminishing regulation became compensable — a fiscal shock that would freeze zoning, environmental review, and preservation programs immediately — or no physical acquisition ever required payment, licensing outright confiscation. Public-works financing, land-use governance, and property markets would all reorganize around whichever replacement line emerged.
% FOUNDING_PROBLEM: Government seizing or occupying private property for public projects without paying the owner — the grievance behind the Clause's adoption, aimed at forced quartering, wartime impressment of lands and goods, and uncompensated condemnation.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: ratification-era records (Madison's drafting notes, state declarations of rights, enumerated colonial grievances) document the physical-dispossession concern; and opponents of this reading — property-rights scholars, dissenting opinions, plaintiff-side litigators — concede the physical-takings core while disputing only whether the Clause stops there. No serious participant, including its critics, attests that the physical core was never the founding problem.
narrative_ontology:disappearance_verdict(takings_clause_boundary__physical_appropriation_reading, world_rearranges).
narrative_ontology:founding_problem_status(takings_clause_boundary__physical_appropriation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(takings_clause_boundary__physical_appropriation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(takings_clause_boundary__physical_appropriation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(takings_clause_boundary__physical_appropriation_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is authored at 0.48: substantial uncompensated value transfers from regulated owners to the public, bounded by the physical-acquisition payment guarantee that still operates daily. Suppression is 0.58 and is a raw structural property, unscaled by power or scope — the doctrine forecloses the compensation alternative itself through threshold dismissals, a structural barrier rather than an interpersonal one, though a thin internalized layer exists (owners socialized to treat regulatory loss as ordinary incidence carry the acceptance with them). Theater is 0.28: the line-drawing work is real, but a growing rhetorical layer of originalist legitimation performs fidelity to a settlement whose practical coverage has shrunk. Accessibility collapse is 0.5 — state constitutions, legislatures, insurance markets, and instrument substitution keep alternatives partly alive. Resistance is 0.62 — a century of sustained litigation, scholarship, and periodic Supreme Court challenges. The temporal series run on one shared seven-point grid (1922–2026): base_extractiveness rises with the regulatory state's expansion and plateaus after the 1990s as the regime matures; theater_ratio rises with originalist rhetorical intensification; suppression_requirement is tracked because the story specifically traces enforcement hardening — as regulatory-takings claim volume grew after the 1980s, the physical-occupation line became the primary dismissal tool and gatekeeping tightened. No cyclical oscillation is asserted; the trajectories are monotonic with a late plateau.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently and the divergence is the finding. From the government seat the arrangement is infrastructure it built and cheaply maintains — a coordination mechanism with a modest payment tail. From the trapped owner seats the same structure operates as an uncompensated transfer, harshest for the wipeout class whose claims are closed at the courthouse door. From the judiciary's administrative seat the boundary is a workload and a legacy — neither collected nor paid. Same-level differentiation matters too: two owner classes at comparable nominal standing sit at different distances from the target end because severity of loss and categorical dismissability differ, and two institutional beneficiaries differ because one holds instrument arbitrage and the other does not. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries drive low directionality: governments sit near the beneficiary end, pushed further by arbitrage-grade exit (taxes, spending, and mandates on other interests substitute for regulation almost anywhere). Land-use agencies sit nearby but with constrained exit — their programs cannot relocate. Declared victims drive high directionality: trapped exit (immobile property, loss priced into any sale) pushes both owner classes toward the full-target end, with the powerless wipeout class nearest 1.0 since its sole channel is categorically closed. The judiciary derives no directionality from beneficiary/victim data and is characterized here qualitatively: it administers without collecting, placing it mid-range rather than at either pole. Only extractiveness scales with directionality and scope; suppression enters the computation unscaled.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — uncompensated physical dispossession — is live: eminent-domain acquisitions and permanent occupations occur continuously and the payment guarantee discharges them. No sunset exists and none should. The tangled_rope claim guards against two symmetrical mislabels: reading the whole arrangement as pure coordination ignores the concentrated uncompensated losses the boundary allocates; reading it as pure extraction ignores the genuine, still-functioning physical-takings guarantee that pays out daily. If the founding problem ever died — acquisition becoming wholly consensual or digital — the boundary would decay toward inertial maintenance, and the flat-lining late-interval measurements would be the leading indicator; nothing in the series currently shows that decay.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment_delta,
    'This constraint is the physical_appropriation_reading of the takings_clause_boundary kernel; what structurally changes if a sibling reading displaces it?',
    'Classify the sibling stories (regulatory_takings_reading, categorical_takings_reading) over the same referent and compare victim sets, epsilon, and per-seat types across the family.',
    'Sibling displacement widens the compensated class to severe-diminution owners, raises the extraction borne by public fiscs, and relieves the wipeout class from wholly unrelieved to partially relieved; the boundary''s coordination function narrows correspondingly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment_delta, conceptual, 'Committer structure: one reading of a three-reading kernel; sibling deltas are structural, not rhetorical.').

omega_variable(
    constructed_line_vs_discovered_limit,
    'Is the physical-only boundary a discovered constitutional limit fixed at ratification, as this reading claims, or a constructed allocation that shelters public fiscs from the cost of their own regulatory programs?',
    'Ratification-era usage corpora and founding-era state practice: did contemporaries apply ''taken'' to non-physical injuries, and did early governments compensate regulatory losses?',
    'If constructed, the boundary is a policy artifact revisable by the institutions that built it and false-natural-law machinery applies; if discovered, it sits nearer the fixed end and revision requires amendment-grade action.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constructed_line_vs_discovered_limit, conceptual, 'Naturalness contest over the boundary''s origin.').

omega_variable(
    uncompensated_loss_severity_distribution,
    'What fraction of regulatory value diminution is severe enough that the sibling readings would classify it as compensable?',
    'Hedonic pricing studies, regulatory-impact accounting, and observed outcomes in jurisdictions that already compensate severe-diminution claims.',
    'A large severe-loss fraction means the boundary concentrates catastrophic uncompensated losses on a small class, sharpening the asymmetry; a small fraction means the exclusion costs are diffuse and modest, closer to ordinary incidence of governance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(uncompensated_loss_severity_distribution, empirical, 'Empirical magnitude of what the line leaves uncompensated.').

omega_variable(
    state_divergence_hollowing,
    'Will divergent state-constitutional readings progressively hollow out the federal boundary''s practical scope?',
    'Track state supreme court adoptions of broader compensation rules and measure cross-state migration of high-regulation-risk assets.',
    'Growing divergence means the federal line persists formally while its bite shrinks — drift toward inertial maintenance; convergence on the federal line consolidates the boundary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_divergence_hollowing, empirical, 'Federalism pressure on the boundary''s effective scope.').

omega_variable(
    authority_grounding_framing_underdetermination,
    'Is the judiciary''s authority over the boundary grounded in lineage (continuity with the ratified text) or in practice (the courts'' own accumulated doctrine as the operative standard)? Two coherent framings of the same authority structure are available.',
    'Observe which source prevails when the two conflict: where precedent is abandoned to restore claimed original meaning, lineage governs; where original-meaning claims yield to precedent, practice governs.',
    'Under a practice framing the interpretation layer is effectively the kernel and revision is ordinary adjudication; under a lineage framing revision requires repudiating the founders'' settlement, raising the cost of change and hardening the boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounding_framing_underdetermination, conceptual, 'Framing under-determination in the commitment-system classification of the interpreting authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__physical_appropriation_reading, 1922, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taki_tr_t1922, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 1922, 0.12).
narrative_ontology:measurement_basis(taki_tr_t1922, observed).
narrative_ontology:measurement(taki_tr_t1937, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 1937, 0.15).
narrative_ontology:measurement_basis(taki_tr_t1937, observed).
narrative_ontology:measurement(taki_tr_t1954, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 1954, 0.18).
narrative_ontology:measurement_basis(taki_tr_t1954, observed).
narrative_ontology:measurement(taki_tr_t1982, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 1982, 0.24).
narrative_ontology:measurement_basis(taki_tr_t1982, observed).
narrative_ontology:measurement(taki_tr_t1992, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 1992, 0.26).
narrative_ontology:measurement_basis(taki_tr_t1992, observed).
narrative_ontology:measurement(taki_tr_t2005, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 2005, 0.27).
narrative_ontology:measurement_basis(taki_tr_t2005, observed).
narrative_ontology:measurement(taki_tr_t2026, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 2026, 0.28).
narrative_ontology:measurement_basis(taki_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(taki_be_t1922, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 1922, 0.28).
narrative_ontology:measurement_basis(taki_be_t1922, observed).
narrative_ontology:measurement(taki_be_t1937, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 1937, 0.33).
narrative_ontology:measurement_basis(taki_be_t1937, observed).
narrative_ontology:measurement(taki_be_t1954, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 1954, 0.38).
narrative_ontology:measurement_basis(taki_be_t1954, observed).
narrative_ontology:measurement(taki_be_t1982, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 1982, 0.44).
narrative_ontology:measurement_basis(taki_be_t1982, observed).
narrative_ontology:measurement(taki_be_t1992, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 1992, 0.46).
narrative_ontology:measurement_basis(taki_be_t1992, observed).
narrative_ontology:measurement(taki_be_t2005, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 2005, 0.47).
narrative_ontology:measurement_basis(taki_be_t2005, observed).
narrative_ontology:measurement(taki_be_t2026, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 2026, 0.48).
narrative_ontology:measurement_basis(taki_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(taki_su_t1922, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 1922, 0.4).
narrative_ontology:measurement_basis(taki_su_t1922, observed).
narrative_ontology:measurement(taki_su_t1937, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 1937, 0.44).
narrative_ontology:measurement_basis(taki_su_t1937, observed).
narrative_ontology:measurement(taki_su_t1954, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 1954, 0.48).
narrative_ontology:measurement_basis(taki_su_t1954, observed).
narrative_ontology:measurement(taki_su_t1982, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 1982, 0.54).
narrative_ontology:measurement_basis(taki_su_t1982, observed).
narrative_ontology:measurement(taki_su_t1992, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 1992, 0.56).
narrative_ontology:measurement_basis(taki_su_t1992, observed).
narrative_ontology:measurement(taki_su_t2005, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 2005, 0.57).
narrative_ontology:measurement_basis(taki_su_t2005, observed).
narrative_ontology:measurement(taki_su_t2026, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 2026, 0.58).
narrative_ontology:measurement_basis(taki_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(takings_clause_boundary__physical_appropriation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(takings_clause_boundary__physical_appropriation_reading, regulatory_takings_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__physical_appropriation_reading, categorical_takings_reading).

% DUAL FORMULATION NOTE:
% 'The Takings Clause boundary' is a colloquial label covering three structurally distinct compensation-trigger claims with different victim sets and different epsilon values. Per the epsilon-invariance principle this corpus decomposes it: the physical_appropriation_reading (this story, narrowest compensated class, owners bear regulatory losses as background risk), the regulatory_takings_reading (adds severe-diminution victims), and the categorical_takings_reading (intermediate: per se treatment for occupations and total wipeouts, balancing for the rest). Each is a separate story with its own beneficiaries, victims, and metrics. Edges run from this upstream story to both siblings because the physical reading is the baseline each sibling cites and relaxes; contamination analysis should treat degradation of the physical reading's credibility as propagating downstream to both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
