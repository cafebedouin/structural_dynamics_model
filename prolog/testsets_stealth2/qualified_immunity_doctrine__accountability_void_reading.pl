% ============================================================================
% CONSTRAINT STORY: qualified_immunity_doctrine__accountability_void_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qualified_immunity_doctrine__accountability_void_reading, []).

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
 *   constraint_id: qualified_immunity_doctrine__accountability_void_reading
 *   human_readable: Qualified Immunity Doctrine — Accountability-Void Reading
 *   domain: legal/constitutional/civil_rights
 *
 * SUMMARY:
 *   This story instantiates one reading of a contested judicial doctrine.
 *   Under the accountability-void reading, qualified immunity as currently
 *   operated functions as a systematic transfer of remedy away from people
 *   whose constitutional rights are violated: because a plaintiff must
 *   identify a prior case clearly establishing the rule the officer broke,
 *   and because courts may resolve immunity before discovery, conceded
 *   violations routinely end without compensation, deterrence, or
 *   acknowledgment. The arrangement is maintained by active judicial
 *   enforcement — dismissal practice, summary reversal of denials, narrowing
 *   of parallel federal remedies — and by the political weight of its
 *   beneficiaries. Family note: the colloquial label decomposes into three
 *   structurally distinct stories (this accountability-void reading, a
 *   protective-scaffold reading, a constitutional-fidelity reading), each a
 *   separate file with its own epsilon and stakeholders, linked via
 *   network.affects_constraints. KEY AGENTS (by structural relationship): -
 *   constitutional_violation_victims: Primary target (powerless/trapped) —
 *   bear the closed remedy path - law_enforcement_officers: Primary
 *   beneficiary (organized/mobile) — personally insulated from damages -
 *   police_unions: Beneficiary (organized/mobile) — representational defense
 *   of the arrangement - municipal_governments: Secondary beneficiary
 *   (institutional/constrained) — reduced indemnity exposure -
 *   federal_judiciary: Agenda setter (institutional/constrained) — creates,
 *   applies, and polices the doctrine - civil_rights_reform_movement:
 *   Excluded voice (organized/constrained) — documents and contests without a
 *   seat in doctrine formation - legal_empirical_researchers: Analytical
 *   observer (institutional/analytical) — measures operation
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qualified_immunity_doctrine__accountability_void_reading, 0.88).
domain_priors:suppression_score(qualified_immunity_doctrine__accountability_void_reading, 0.8).
domain_priors:theater_ratio(qualified_immunity_doctrine__accountability_void_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qualified_immunity_doctrine__accountability_void_reading, snare).
narrative_ontology:human_readable(qualified_immunity_doctrine__accountability_void_reading, "Qualified Immunity Doctrine — Accountability-Void Reading").
narrative_ontology:topic_domain(qualified_immunity_doctrine__accountability_void_reading, "legal/constitutional/civil_rights").

domain_priors:requires_active_enforcement(qualified_immunity_doctrine__accountability_void_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qualified_immunity_doctrine__accountability_void_reading, 'a5ae7bfb-84be-4d34-b2d1-9424664e3392').
narrative_ontology:cs_kernel_codification('a5ae7bfb-84be-4d34-b2d1-9424664e3392', formalized).
narrative_ontology:cs_authority_grounding('a5ae7bfb-84be-4d34-b2d1-9424664e3392', lineage).
narrative_ontology:cs_interpretation_layer_present('a5ae7bfb-84be-4d34-b2d1-9424664e3392').
narrative_ontology:cs_reading_relation('a5ae7bfb-84be-4d34-b2d1-9424664e3392', qualified_immunity_doctrine__protective_scaffold_reading, coexists_with).
narrative_ontology:cs_reading_relation('a5ae7bfb-84be-4d34-b2d1-9424664e3392', qualified_immunity_doctrine__constitutional_fidelity_reading, influences).
narrative_ontology:cs_axiom('a5ae7bfb-84be-4d34-b2d1-9424664e3392', foundational, constitutional_rights_require_remedies).
narrative_ontology:cs_axiom_status(constitutional_rights_require_remedies, holdable).
narrative_ontology:cs_axiom_grounding('a5ae7bfb-84be-4d34-b2d1-9424664e3392', constitutional_rights_require_remedies, deontological).
narrative_ontology:cs_axiom('a5ae7bfb-84be-4d34-b2d1-9424664e3392', foundational, impunity_is_systematic_not_incidental).
narrative_ontology:cs_axiom_status(impunity_is_systematic_not_incidental, holdable).
narrative_ontology:cs_axiom_grounding('a5ae7bfb-84be-4d34-b2d1-9424664e3392', impunity_is_systematic_not_incidental, empirically_contingent).
narrative_ontology:cs_reference_frame('a5ae7bfb-84be-4d34-b2d1-9424664e3392', section_1983_remedial_baseline).
narrative_ontology:cs_drift_state('a5ae7bfb-84be-4d34-b2d1-9424664e3392', contemporary, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('a5ae7bfb-84be-4d34-b2d1-9424664e3392', '').
narrative_ontology:cs_kernel_id(qualified_immunity_doctrine__accountability_void_reading, qualified_immunity_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__accountability_void_reading, law_enforcement_officers).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__accountability_void_reading, police_unions).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__accountability_void_reading, municipal_governments).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__accountability_void_reading, constitutional_violation_victims).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__accountability_void_reading, municipal_governments).
narrative_ontology:constraint_vindicates(qualified_immunity_doctrine__accountability_void_reading, official_immunity_common_law_tradition).
narrative_ontology:constraint_vindicates(qualified_immunity_doctrine__accountability_void_reading, good_faith_official_conduct_presumption).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Experience searches, seizures, arrests, or uses of force that courts may later deem unconstitutional. Their suit against the responsible officer is typically dismissed before discovery because no prior case clearly established the rule; alternative paths — state tort claims, claims against municipalities — carry their own independent thresholds. Once harmed they cannot exit the injury or the governing law; their remedy path is what the arrangement closes.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, constitutional_violation_victims, payer,
    powerless, biographical, trapped, national).

% Patrol and detain under color of law. When a court finds their conduct violated a constitutional right, the doctrine bars damages recovery against them personally unless prior case law clearly forbade the specific act, so personal judgment exposure almost never materializes; departments indemnify in the rare exception. Exit looks ordinary: they may change departments or leave policing without carrying any of the arrangement's costs.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, law_enforcement_officers, beneficiary,
    organized, biographical, mobile, national).

% Negotiate contracts and lobby legislatures on members' behalf. The doctrine shrinks the civil-litigation exposure they would otherwise bargain around, and they defend it publicly and politically. Their stake is collective and representational; they can redirect organizing effort to other issues if the arrangement changed.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, police_unions, beneficiary,
    organized, biographical, mobile, national).

% Employ the officers and pay most judgments and settlements through indemnification. The doctrine reduces the population of recoverable claims and the size of settlements, easing budgets and insurance premiums; they also bear residual payout and reputational costs in the cases that survive. They cannot exit the legal order containing the doctrine and would face the underlying claims either way.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, municipal_governments, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(qualified_immunity_doctrine__accountability_void_reading, municipal_governments, payer).

% Creates, applies, and polices the doctrine: Supreme Court majorities define its scope, appellate panels apply the clearly-established test, and district judges grant dismissal. Judges who deny immunity face summary reversal; precedent binds the bench collectively, so no individual judge can exit the administration of the arrangement.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Litigators, advocacy organizations, and academics who document dismissal patterns and press for statutory replacement. They testify and publish but hold no vote in doctrine formation; their access runs through the same courts that apply the bar.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, civil_rights_reform_movement, excluded,
    organized, biographical, constrained, national).

% Code outcomes, measure filing and dismissal rates, and publish findings on how the doctrine operates in practice. They collect nothing from it and bear none of its costs.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, legal_empirical_researchers, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qualified_immunity_doctrine__accountability_void_reading, law_enforcement_officers).
narrative_ontology:fixing_cost_class(qualified_immunity_doctrine__accountability_void_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates decisions about officer accountability inside employing institutions and the judiciary: individual officers are spared personal damages exposure, and claims against them are filtered by courts before evidence gathering, leaving discipline to departments and liability to employers.
% TRANSFER_FUNCTION: Moves remedy, deterrence, and acknowledgment away from people whose constitutional rights were violated: their claims terminate before discovery, officers' personal assets and careers are insulated from judgment, and dispute resolution shifts from open court to internal departmental process.
% ABSENT_VOICES: People whose claims were dismissed on immunity grounds before any merits hearing rarely appear anywhere — their accounts enter the record only as case captions. Communities experiencing recurring unconstitutional policing have no seat in doctrine formation; reform legislators testify but do not decide judge-made doctrine.
% DISAPPEARANCE_RATIONALE: Section-1983 dockets would reprice immediately: filings against individual officers would survive motion practice in materially larger numbers, indemnification and insurance markets would absorb new exposure, departments would adjust training and supervision toward avoiding personal-judgment risk, and the pipeline of constitutional-violation claims reaching juries would reopen.
% FOUNDING_PROBLEM: Shielding government officials from the burden of defending bad-faith suits while the modern law of constitutional torts was young — articulated in Pierson v. Ray (1967) amid surging civil-rights litigation and hardened in Harlow v. Fitzgerald (1982) to spare officials discovery-driven harassment.
% FOUNDING_PROBLEM_CORROBORATION: Judicial dissents cataloguing immunity grants for egregious conduct, peer-reviewed empirical studies of section-1983 outcomes, and state legislatures that replaced the doctrine with statutory standards (Colorado 2020, New Mexico 2021) all attest from outside the benefiting parties that the founding problem no longer describes the doctrine's operation; the benefiting parties and the judiciary majority attest it remains live.
narrative_ontology:disappearance_verdict(qualified_immunity_doctrine__accountability_void_reading, world_rearranges).
narrative_ontology:founding_problem_status(qualified_immunity_doctrine__accountability_void_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qualified_immunity_doctrine__accountability_void_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qualified_immunity_doctrine__accountability_void_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qualified_immunity_doctrine__accountability_void_reading, 0.88, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qualified_immunity_doctrine__accountability_void_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qualified_immunity_doctrine__accountability_void_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qualified_immunity_doctrine__accountability_void_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high (0.88) because the bar operates near-absolutely: claims die before discovery whenever no prior case clearly established the rule, so the remedy value of a conceded violation approaches zero for the victim. Suppression (0.80) is authored as a raw structural property — unscaled by power or scope; only extractiveness is scaled by the engine — and tracks the enforcement machinery: summary dismissal, summary reversal of denials, narrowing of parallel federal remedies. Theater (0.55) reflects the growing share of opinion-writing devoted to ritualized clearly-established analysis and justificatory rhetoric while personal liability remains vanishingly rare behind indemnification. Accessibility collapse (0.62): alternatives persist (municipal-liability claims, state torts, equitable relief) but each carries independent thresholds, so understanding the arrangement collapses most practical exits without eliminating all. Resistance (0.70): five decades of scholarship, repeated federal repeal bills, state statutory replacements, and intra-court dissent; aggregation vehicles (advocacy organizations, class litigation) give otherwise powerless victims partial coalition power, which is why resistance is this high despite a powerless payer seat. The measurement series share one grid — every tracked metric authored at every point 0–56 by 8 — and the trajectories are ratchets, not cycles: the Harlow-era objective standard (grid point 16) steps extraction and enforcement up together, and Pearson-era practice (points 40–48) hardens application further. The suppression_requirement series is authored because the story specifically traces enforcement-capacity hardening, not merely shifting extraction. fixing_cost is authored 'prohibitive': repeal is legislatively trivial in form but has failed repeatedly against concentrated beneficiary opposition, while the benefits of removal accrue diffusely to future victims.
 *
 * PERSPECTIVAL GAP:
 *   From the officer and union seats the arrangement presents as settled background law protecting public service; from the victim seat the same doctrine is a closed door encountered at the moment of injury; from the bench it is a doctrine to administer under reversal risk. The engine computes per-seat classifications from power, exit, and declared position; the divergence between the beneficiary seats' subsidized experience and the payer seat's trapped experience is the measurement this corpus exists to take. The authored claim (snare) is the generating seat's structural belief and is deliberately left unreconciled to any predicted per-seat output.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows: officers, unions, and municipalities sit at the beneficiary pole (d near 0) — the arrangement subsidizes them with insulation and budget relief; officers' mobile exit pushes them toward arbitrage-grade benefit, while municipalities' constrained exit moderates theirs. Victims sit at the target pole (d near 1), amplified by trapped exit: no remedy path, no exit from the injury or the governing law, national scope raising verification difficulty. The judiciary holds an administrative seat with no declared beneficiary or victim position; its constrained exit (collective precedent commitment) keeps it engaged without making it a collector. No directionality overrides are authored: the derivation chain from beneficiary/victim declarations plus exit options reproduces these relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding-problem interview separates origin from operation. The doctrine was built for a real filtering problem — bad-faith suits against officials during constitutional-tort law's infancy — and this reading attests that problem is at best contested as a description of current operation. Because status is authored 'contested' rather than 'dead', the mismatch consumer (status=dead x verdict=world_rearranges) correctly does not fire the zombie flag on this story alone, but the pairing is flagged for cross-check against the computed theater path. The classification discipline cuts both ways: it blocks the scaffold mislabel (reading a filtering cover story as the whole function) and equally blocks the opposite error of denying that any filtering ever occurred. Resolution here is empirical, not rhetorical: the Colorado and New Mexico statutory replacements supply counterfactual jurisdictions for the deterrence omega.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is the accountability_void_reading of the qualified_immunity_doctrine kernel: which structural facts would change if a sibling reading (protective_scaffold_reading, constitutional_fidelity_reading) were instantiated instead?',
    'Compile the sibling stories and compare victim-set membership, epsilon_base, and computed per-seat types; convergence on victim presence across readings would indicate the delta is framing rather than structure.',
    'Under the scaffold reading the victim set thins (only bad-faith conduct counts as unprotected), epsilon falls toward coordination-cost levels, and the computed type shifts toward rope/scaffold; under the fidelity reading the defect is located in authorization rather than operative transfer, changing which remedies (abolition versus recalibration) follow.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer-frame contest: one kernel, three readings with divergent victim sets and epsilon.').

omega_variable(
    deterrence_and_frivolous_suit_empirics,
    'Does eliminating or narrowing the doctrine measurably increase non-meritorious litigation against officers, and does retaining it measurably reduce officer misconduct?',
    'Difference-in-differences across jurisdictions that statutorily replaced the doctrine (Colorado 2020, New Mexico 2021) against matched controls: filing volumes, dismissal rates, settlement values, and misconduct indicators.',
    'If filings stay flat and misconduct indicators fall, the filtering justification weakens and the measured transfer stands closer to pure rent; if filings surge, part of the measured transfer is the price of a real filtering function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_and_frivolous_suit_empirics, empirical, 'Whether the doctrine''s filtering function is real or cover.').

omega_variable(
    chilled_filing_undercount,
    'How much of the constraint''s suppressive force operates upstream of the courtroom — potential claimants who never file because they anticipate dismissal — versus in the doctrinal dismissal rate itself?',
    'Compare rights-violation incidence against section-1983 filing rates; attorney screening interviews; natural experiments where local publicity about immunity grants changes filing propensity.',
    'If chilling dominates, the scalar suppression understates effective suppression and the victim population is far larger than the docket shows; if dismissal-rate suppression dominates, the measured series already captures the force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chilled_filing_undercount, empirical, 'Structural versus anticipatory (chilled) suppression.').

omega_variable(
    clearly_established_indeterminacy,
    'Is the ''clearly established'' standard determinate enough to give officers fair notice — the doctrine''s own stated legitimacy condition — or is it applied retrospectively and unevenly across circuits?',
    'Inter-circuit variance studies, reversal-rate analysis of immunity denials, and coding of published immunity opinions for reliance on factually distant precedent.',
    'If indeterminate, the fair-notice justification fails on the doctrine''s own terms and the performative share of enforcement activity rises; if determinate, part of the theater_ratio reflects genuine notice-giving.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(clearly_established_indeterminacy, conceptual, 'Whether the doctrine''s internal legitimacy condition holds.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qualified_immunity_doctrine__accountability_void_reading, 0, 56).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qual_tr_t0, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(qual_tr_t0, observed).
narrative_ontology:measurement(qual_tr_t8, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement_basis(qual_tr_t8, observed).
narrative_ontology:measurement(qual_tr_t16, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement_basis(qual_tr_t16, observed).
narrative_ontology:measurement(qual_tr_t24, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 24, 0.36).
narrative_ontology:measurement_basis(qual_tr_t24, observed).
narrative_ontology:measurement(qual_tr_t32, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 32, 0.42).
narrative_ontology:measurement_basis(qual_tr_t32, observed).
narrative_ontology:measurement(qual_tr_t40, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 40, 0.47).
narrative_ontology:measurement_basis(qual_tr_t40, observed).
narrative_ontology:measurement(qual_tr_t48, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 48, 0.52).
narrative_ontology:measurement_basis(qual_tr_t48, observed).
narrative_ontology:measurement(qual_tr_t56, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 56, 0.55).
narrative_ontology:measurement_basis(qual_tr_t56, observed).

% Extraction over time
narrative_ontology:measurement(qual_be_t0, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(qual_be_t0, observed).
narrative_ontology:measurement(qual_be_t8, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement_basis(qual_be_t8, observed).
narrative_ontology:measurement(qual_be_t16, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement_basis(qual_be_t16, observed).
narrative_ontology:measurement(qual_be_t24, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement_basis(qual_be_t24, observed).
narrative_ontology:measurement(qual_be_t32, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 32, 0.72).
narrative_ontology:measurement_basis(qual_be_t32, observed).
narrative_ontology:measurement(qual_be_t40, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 40, 0.78).
narrative_ontology:measurement_basis(qual_be_t40, observed).
narrative_ontology:measurement(qual_be_t48, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 48, 0.84).
narrative_ontology:measurement_basis(qual_be_t48, observed).
narrative_ontology:measurement(qual_be_t56, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 56, 0.88).
narrative_ontology:measurement_basis(qual_be_t56, observed).

% Suppression requirement over time
narrative_ontology:measurement(qual_su_t0, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(qual_su_t0, observed).
narrative_ontology:measurement(qual_su_t8, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 8, 0.38).
narrative_ontology:measurement_basis(qual_su_t8, observed).
narrative_ontology:measurement(qual_su_t16, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 16, 0.5).
narrative_ontology:measurement_basis(qual_su_t16, observed).
narrative_ontology:measurement(qual_su_t24, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 24, 0.58).
narrative_ontology:measurement_basis(qual_su_t24, observed).
narrative_ontology:measurement(qual_su_t32, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 32, 0.64).
narrative_ontology:measurement_basis(qual_su_t32, observed).
narrative_ontology:measurement(qual_su_t40, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement_basis(qual_su_t40, observed).
narrative_ontology:measurement(qual_su_t48, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 48, 0.76).
narrative_ontology:measurement_basis(qual_su_t48, observed).
narrative_ontology:measurement(qual_su_t56, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 56, 0.8).
narrative_ontology:measurement_basis(qual_su_t56, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qualified_immunity_doctrine__accountability_void_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__accountability_void_reading, protective_scaffold_reading).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__accountability_void_reading, constitutional_fidelity_reading).

% DUAL FORMULATION NOTE:
% Decomposition per the epsilon-invariance principle: the colloquial label 'qualified immunity' covers three structurally distinct claims with different epsilon, victim sets, and failure modes. This file is the accountability-void member (high epsilon, victims with no remedy path); the protective-scaffold member authors epsilon near coordination cost with a thin victim set; the constitutional-fidelity member locates the defect in authorization rather than magnitude of transfer. Each sibling is a separate file; edges here run to both, and contamination propagation treats a purity shift in this reading as pressure on the scaffold reading's legitimacy conditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
