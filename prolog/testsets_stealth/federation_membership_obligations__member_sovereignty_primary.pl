% ============================================================================
% CONSTRAINT STORY: federation_membership_obligations__member_sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_obligations__member_sovereignty_primary, []).

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
 *   constraint_id: federation_membership_obligations__member_sovereignty_primary
 *   human_readable: Member-State Welfare Closure Authority over Mobile EU Workers
 *   domain: political economy/federalism/migration policy
 *
 * SUMMARY:
 *   This file instantiates ONE reading — member_sovereignty_primary — of the
 *   contested kernel federation_membership_obligations: national welfare
 *   states retain closure authority, and free movement is conditional on
 *   labor market protection and welfare system sustainability. The standing
 *   arrangement under contest (the fixed ε referent across all readings of
 *   this kernel) is the closure regime itself: mobile EU workers who exercise
 *   free movement and pay taxes and social contributions are excluded from
 *   parts of the receiving state's welfare system — during qualifying
 *   periods, while job-seeking, or while economically inactive — under
 *   residence and habitual-residence tests administered by national agencies
 *   and enacted by member state legislatures. By this reading's own lights
 *   the closure is largely legitimate protection, so ε is authored moderate;
 *   an integration-primary reading of the SAME referent would author ε
 *   substantially higher. The kernel decomposes into three structurally
 *   distinct readings (this one, integration_primary, selective_solidarity)
 *   with different victim sets and authority allocations; per the
 *   ε-invariance principle they are separate stories linked through
 *   network.affects_constraints, not one story with a measurement parameter.
 *   The claim and the metrics are authored independently: claimed_type states
 *   what this reading holds structurally true; the metrics describe the
 *   regime's actual operation.
 *
 * KEY AGENTS:
 *   - member_state_legislatures: agenda-setter (institutional/arbitrage) — retain statutory veto authority over welfare access; the boundary the constraint maintains is the one they draw
 *   - national_treasuries_and_welfare_agencies: agenda-setter and fiscal beneficiary (institutional/arbitrage) — administer residence tests; collect contributions from workers they partially exclude
 *   - domestic_labor_forces: primary beneficiary (organized/mobile) — full welfare access and political protection; their electoral weight sustains the closure
 *   - mobile_eu_workers: primary payer (moderate/constrained) — work and contribute but are denied parts of the welfare package; exit is costly
 *   - economically_inactive_migrants: concentrated payer (powerless/constrained) — jobseekers and inactive movers face the hardest exclusion and removal risk
 *   - cross_border_families: payer (moderate/trapped) — denied family benefits across coordination gaps; exit would split households
 *   - sending_state_governments: excluded (organized/analytical) — would object to unequal treatment of their citizens; no seat in receiving-state eligibility decisions
 *   - migrant_rights_organizations: excluded (moderate/analytical) — litigate for equal treatment; absent from the legislative conversation
 *   - european_commission: analytical observer (institutional/analytical) — polices free movement compliance from the rival reading's institutional seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_obligations__member_sovereignty_primary, 0.48).
domain_priors:suppression_score(federation_membership_obligations__member_sovereignty_primary, 0.55).
domain_priors:theater_ratio(federation_membership_obligations__member_sovereignty_primary, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, extractiveness, 0.48).
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__member_sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__member_sovereignty_primary, "Member-State Welfare Closure Authority over Mobile EU Workers").
narrative_ontology:topic_domain(federation_membership_obligations__member_sovereignty_primary, "political economy/federalism/migration policy").

domain_priors:requires_active_enforcement(federation_membership_obligations__member_sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_obligations__member_sovereignty_primary, '84f76c62-187b-4ea6-bc1e-fa47c07bd064').
narrative_ontology:cs_kernel_codification('84f76c62-187b-4ea6-bc1e-fa47c07bd064', fixed_text).
narrative_ontology:cs_authority_grounding('84f76c62-187b-4ea6-bc1e-fa47c07bd064', lineage).
narrative_ontology:cs_interpretation_layer_present('84f76c62-187b-4ea6-bc1e-fa47c07bd064').
narrative_ontology:cs_reading_relation('84f76c62-187b-4ea6-bc1e-fa47c07bd064', federation_membership_obligations__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('84f76c62-187b-4ea6-bc1e-fa47c07bd064', federation_membership_obligations__selective_solidarity, influences).
narrative_ontology:cs_axiom('84f76c62-187b-4ea6-bc1e-fa47c07bd064', foundational, national_welfare_self_determination).
narrative_ontology:cs_axiom_status(national_welfare_self_determination, holdable).
narrative_ontology:cs_axiom_grounding('84f76c62-187b-4ea6-bc1e-fa47c07bd064', national_welfare_self_determination, deontological).
narrative_ontology:cs_axiom('84f76c62-187b-4ea6-bc1e-fa47c07bd064', foundational, mobility_conditioned_on_sustainability).
narrative_ontology:cs_axiom_status(mobility_conditioned_on_sustainability, holdable).
narrative_ontology:cs_axiom_grounding('84f76c62-187b-4ea6-bc1e-fa47c07bd064', mobility_conditioned_on_sustainability, instrumental).
narrative_ontology:cs_reference_frame('84f76c62-187b-4ea6-bc1e-fa47c07bd064', national_welfare_closure_authority).
narrative_ontology:cs_drift_state('84f76c62-187b-4ea6-bc1e-fa47c07bd064', contemporary_post_enlargement_jurisprudence, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('84f76c62-187b-4ea6-bc1e-fa47c07bd064', '').
narrative_ontology:cs_kernel_id(federation_membership_obligations__member_sovereignty_primary, federation_membership_obligations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_obligations__member_sovereignty_primary, member_state_legislatures).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__member_sovereignty_primary, domestic_labor_forces).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__member_sovereignty_primary, national_treasuries_and_welfare_agencies).
narrative_ontology:constraint_victim(federation_membership_obligations__member_sovereignty_primary, mobile_eu_workers).
narrative_ontology:constraint_victim(federation_membership_obligations__member_sovereignty_primary, economically_inactive_migrants).
narrative_ontology:constraint_victim(federation_membership_obligations__member_sovereignty_primary, cross_border_families).
narrative_ontology:constraint_vindicates(federation_membership_obligations__member_sovereignty_primary, member_state_social_competence_doctrine).
narrative_ontology:constraint_vindicates(federation_membership_obligations__member_sovereignty_primary, welfare_sustainability_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain statutory authority to define residence conditions, habitual-residence tests, and benefit eligibility for mobile EU citizens, and can restrict or extend welfare access by ordinary legislation. They face recurring electoral pressure over migration and welfare spending, and the closure authority is a lever they actively use and defend against Union-level review.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, member_state_legislatures, agenda_setter,
    institutional, biographical, arbitrage, national).

% Administer the right-to-reside and habitual-residence assessments, decide claims, deny benefits, and recover overpayments. They collect income tax, VAT, and social contributions from mobile workers whom they simultaneously exclude from parts of the benefit package, and they report the fiscal position that sustains the sustainability argument.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, national_treasuries_and_welfare_agencies, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_obligations__member_sovereignty_primary, national_treasuries_and_welfare_agencies, beneficiary).

% Hold full access to the welfare system and are the politically protected class the closure is framed around. Their unions and electoral weight supply the mandate legislatures cite; most do not move, and their exit option of relocating abroad is real but rarely exercised.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, domestic_labor_forces, beneficiary,
    organized, biographical, mobile, national).

% Exercise free movement, take jobs, pay income tax and social contributions in the receiving state, and are denied parts of the benefit package during qualifying periods or while between jobs. Their employment, housing, and often family life sit in the receiving state; leaving means abandoning all of it, so exit is available but costly.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, mobile_eu_workers, payer,
    moderate, biographical, constrained, continental).

% Jobseekers and economically inactive movers face the hardest edge of the closure: benefit denial, no safety net, and exposure to removal proceedings. They are transient, dispersed across jurisdictions, and legally precarious, which makes collective organization difficult; their day-to-day horizon is survival, not institutional reform.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, economically_inactive_migrants, payer,
    powerless, immediate, constrained, regional).

% Households spanning two member states — one partner working, children schooled, benefits claimed across a coordination gap — are denied child benefits and family supplements in one state while contributing in the other. Their exit would mean splitting the household or disrupting children's schooling, which most cannot do.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, cross_border_families, payer,
    moderate, generational, trapped, regional).

% Governments of the member states whose citizens emigrate. Their nationals are the population the closure conditions; they would object to unequal treatment of their own citizens abroad but hold no seat in receiving-state eligibility decisions. Their leverage is diplomatic and runs through Council votes on coordination rules, not through the statutes that draw the line.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, sending_state_governments, excluded,
    organized, generational, analytical, national).

% Litigate equal-treatment claims and document the effects of residence tests on mobile workers and their families. They are absent from the legislative conversations where eligibility statutes are written; their channel of influence is the courtroom, not the parliament, and they hold no vote on the boundary the constraint maintains.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, migrant_rights_organizations, excluded,
    moderate, generational, analytical, continental).

% Polices free movement compliance and brings infringement actions against residence tests it reads as discriminatory, holding the rival mobility-constitutive position institutionally. It can litigate and negotiate but cannot directly override a national eligibility statute; its role is external review of the boundary this constraint maintains.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, european_commission, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_obligations__member_sovereignty_primary, national_treasuries_and_welfare_agencies).
narrative_ontology:fixing_cost_class(federation_membership_obligations__member_sovereignty_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real collective-action problem: welfare systems are nationally funded and nationally governed, so unconditioned cross-border access would create fiscal free-rider exposure and unmanaged labor-market adjustment costs. The closure authority coordinates expectations between domestic contributors and incoming mobile populations and preserves the national fiscal basis on which each welfare state's solidarity is built.
% TRANSFER_FUNCTION: Moves welfare-access security and its fiscal backing from mobile EU workers — who pay income tax, VAT, and social contributions in the receiving state while being excluded from parts of the benefit package — to domestic beneficiary pools and national budgets; and moves final decision authority over welfare boundaries from Union institutions to member state legislatures.
% ABSENT_VOICES: Mobile workers and economically inactive migrants are absent from the legislative conversations where eligibility statutes are written. Sending state governments would object to unequal treatment of their citizens but hold no seat in receiving-state eligibility decisions. Migrant rights organizations reach courts but not legislatures. The beneficiary coalition's unanimity is partly an artifact of who is in the room when the line is drawn.
% DISAPPEARANCE_RATIONALE: If closure authority vanished overnight, welfare access rules would reorganize around Union-level coordination or portability arrangements, member states would lose a sovereignty lever they actively use, the domestic political coalitions built on the closure would re-form, and mobile workers' claims would shift from discretionary national tests to enforceable rights. The arrangement is load-bearing for both the welfare systems' fiscal self-understanding and the political settlement around migration.
% FOUNDING_PROBLEM: Reconcile nationally funded welfare solidarity with single-market free movement: postwar welfare states were built on a closed national membership whose contributions funded national benefits, and enlargement plus mobility threatened that fiscal and political basis. The arrangement was built to let member states protect welfare sustainability and domestic labor markets while formally honoring mobility rights.
% FOUNDING_PROBLEM_CORROBORATION: Member state fiscal ministries and domestic trade unions attest the problem is live, citing electoral mandates and fiscal projections. From outside the benefiting parties, European Commission impact assessments and academic fiscal studies finding intra-EU mobile workers to be net contributors contest the empirical premise. No neutral source attests the founding problem unambiguously — which is itself the signal that its status is contested rather than settled.
narrative_ontology:disappearance_verdict(federation_membership_obligations__member_sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_obligations__member_sovereignty_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_obligations__member_sovereignty_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership_obligations__member_sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_obligations__member_sovereignty_primary, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_obligations__member_sovereignty_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_obligations__member_sovereignty_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_obligations__member_sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.48 — moderate — because the referent is fixed (the standing closure regime) while the value is reading-indexed: this reading endorses the closure as legitimate, and even on its own terms it must concede that workers who contribute while being denied reciprocity are genuinely extracted from, and that the denial falls hardest on those least able to absorb it. An integration-primary reading of the same regime would author ε near 0.75; that divergence across a shared referent is the corpus's measurement, not an inconsistency. Suppression (0.55) is a raw structural property — administrative tests, benefit denial, removal powers — and is deliberately NOT scaled by power or scope; only extractiveness is scaled in the engine's computation. Theater_ratio (0.40) reflects a real fiscal-coordination and labor-protection function coexisting with a substantial symbolic component: enforcement rhetoric directed at 'welfare migration' that fiscal studies find to be a net contributor is largely sovereignty assertion, which is why the series rises through the 2014 peak and eases slightly afterward. Accessibility_collapse (0.45): alternatives persist — meeting residence conditions, moving on, returning home — but each is costly, so alternatives are narrowed rather than eliminated. Resistance (0.60): Commission infringement actions, CJEU litigation, NGO challenges, and sending-state objections are continuous and real. Claimed type is tangled_rope because both a genuine coordination function and asymmetric extraction through the same structure are present, and the arrangement requires active enforcement to hold; this claim is stated independently of the metric values. Coalition note for the powerless seats: economically_inactive_migrants cannot easily aggregate — they are transient, dispersed, and legally precarious — so their coalition power runs through migrant_rights_organizations' litigation channel rather than through electoral or legislative leverage.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seats should compute differently, and the divergence is the object of measurement. From the legislature and treasury seats the arrangement is democratic self-government: the community that funds the system governs access to it, and the conditions are proportionate sustainability tools. From the mobile worker's seat the same structure operates as denial of earned reciprocity — contributions paid, benefits withheld — and from the inactive migrant's seat as outright exclusion with removal risk. Inter-institutionally, the European Commission experiences the constraint from the rival reading's seat: what member states call legitimate competence, the Commission litigates as discrimination. Among same-level actors, a domestic worker and a mobile worker of similar skill and income sit at the same individual power level but on opposite sides of the boundary — the differentiator is nationality-correlated residence status, not power. The engine computes per-seat classifications from the structural data; nothing in the authored claim adjudicates the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: member_state_legislatures hold the veto authority and national_treasuries_and_welfare_agencies collect the fiscal residue of contributions-without-reciprocity — both derive low d from their declarations and arbitrage-grade control. domestic_labor_forces receive full access and political protection (low d, their mobile exit damps it further). Victims: mobile_eu_workers derive high d — they contribute and are partially excluded with costly exit; economically_inactive_migrants sit nearest the full-target end — concentrated exclusion, least exit capacity; cross_border_families derive high d — trapped by schooling and dual households. The structural derivation from these beneficiary/victim declarations plus exit options is sufficient; no directionality_overrides are authored because no seat's true relationship diverges from what the derivation produces. sending_state_governments and migrant_rights_organizations are excluded rather than coordinated — their absence from the eligibility conversation is part of what the enforcement machinery maintains. The european_commission observes from outside, holding the sibling reading institutionally.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — protecting nationally funded welfare solidarity while honoring mobility — is contested rather than dead: member states and unions attest liveness through electoral mandates and fiscal projections, while Commission studies and academic fiscal analyses find intra-EU mobile workers to be net contributors, contesting the empirical premise. Classifying this as tangled_rope prevents two mislabels. A pure snare reading would erase the real coordination function: welfare systems are genuinely nationally governed, and unconditioned cross-border access would create fiscal externalities that some coordination mechanism must manage. A pure rope reading would erase the asymmetric extraction: contributors denied reciprocity along nationality-correlated lines, with the hardest exclusion falling on the powerless. The mismatch consumer should note founding_problem_status='contested' against disappearance_verdict='world_rearranges' — the arrangement persists because arrangements depend on it, not because its founding problem is settled; that combination is the live capture/zombie question this story hands to the engine.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_allocation_omega,
    'This constraint is one reading (member_sovereignty_primary) of the federation_membership_obligations kernel: which authority allocation should the kernel instantiate — national legislative veto over welfare access (this reading), Union mobility rights (integration_primary), or contributory tiering (selective_solidarity)?',
    'Treaty revision, a decisive CJEU doctrinal line on residence-test proportionality, or a formal constitutional settlement on welfare competence allocation between Union and member state levels.',
    'Under integration_primary the victim set re-specifies to the member state welfare boundaries themselves and ε for the closure regime is re-authored substantially higher; under selective_solidarity the beneficiary/victim line re-draws by contribution history rather than nationality, moving long-term contributing mobile workers into the beneficiary set and non-contributors into the excluded set.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_allocation_omega, conceptual, 'Committer structure: which sibling reading of the membership kernel this constraint competes with and what each would change structurally.').

omega_variable(
    welfare_migration_fiscal_basis,
    'Do mobile EU citizens impose net fiscal strain on receiving welfare systems, or are they net contributors — is the sustainability premise of the closure empirically real?',
    'Administrative data linking tax and contribution payments to benefit claims by origin cohort across receiving states; quasi-experimental evidence from benefit-access reforms.',
    'If mobile workers are net contributors, the coordination justification weakens, the extraction share of the arrangement rises (toward snare), and theater_ratio is re-authored upward; if genuine strain exists in specific benefit categories, part of the measured extraction is the price of real coordination rather than rent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_migration_fiscal_basis, empirical, 'Whether the welfare-sustainability premise of the closure is empirically grounded or predominantly symbolic.').

omega_variable(
    conditionality_line_within_reading,
    'Where does this reading''s own conditionality line fall — equal treatment for workers only, exclusion of jobseekers as well, or exclusion of all economically inactive movers?',
    'Member state legislative practice and CJEU jurisprudence on residence-condition proportionality: the exclusion-upholding line (Dano, Alimanovic) against the proportionality-review line (Brey, Lassal).',
    'A harder line (excluding jobseekers) enlarges the victim set and raises the effective extraction borne by powerless seats; a softer line (workers only) shrinks the victim set and moves this reading''s structure toward selective_solidarity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conditionality_line_within_reading, conceptual, 'The reading''s internal boundary: which mobile populations the closure legitimately covers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__member_sovereignty_primary, 1993, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t1993, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 1993, 0.25).
narrative_ontology:measurement_basis(fede_tr_t1993, observed).
narrative_ontology:measurement(fede_tr_t1999, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 1999, 0.28).
narrative_ontology:measurement_basis(fede_tr_t1999, observed).
narrative_ontology:measurement(fede_tr_t2004, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 2004, 0.33).
narrative_ontology:measurement_basis(fede_tr_t2004, observed).
narrative_ontology:measurement(fede_tr_t2009, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 2009, 0.36).
narrative_ontology:measurement_basis(fede_tr_t2009, observed).
narrative_ontology:measurement(fede_tr_t2014, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 2014, 0.42).
narrative_ontology:measurement_basis(fede_tr_t2014, observed).
narrative_ontology:measurement(fede_tr_t2019, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 2019, 0.44).
narrative_ontology:measurement_basis(fede_tr_t2019, observed).
narrative_ontology:measurement(fede_tr_t2025, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 2025, 0.4).
narrative_ontology:measurement_basis(fede_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(fede_be_t1993, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 1993, 0.32).
narrative_ontology:measurement_basis(fede_be_t1993, observed).
narrative_ontology:measurement(fede_be_t1999, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 1999, 0.35).
narrative_ontology:measurement_basis(fede_be_t1999, observed).
narrative_ontology:measurement(fede_be_t2004, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 2004, 0.4).
narrative_ontology:measurement_basis(fede_be_t2004, observed).
narrative_ontology:measurement(fede_be_t2009, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 2009, 0.42).
narrative_ontology:measurement_basis(fede_be_t2009, observed).
narrative_ontology:measurement(fede_be_t2014, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 2014, 0.46).
narrative_ontology:measurement_basis(fede_be_t2014, observed).
narrative_ontology:measurement(fede_be_t2019, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 2019, 0.47).
narrative_ontology:measurement_basis(fede_be_t2019, observed).
narrative_ontology:measurement(fede_be_t2025, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 2025, 0.48).
narrative_ontology:measurement_basis(fede_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t1993, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 1993, 0.4).
narrative_ontology:measurement_basis(fede_su_t1993, observed).
narrative_ontology:measurement(fede_su_t1999, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 1999, 0.44).
narrative_ontology:measurement_basis(fede_su_t1999, observed).
narrative_ontology:measurement(fede_su_t2004, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 2004, 0.5).
narrative_ontology:measurement_basis(fede_su_t2004, observed).
narrative_ontology:measurement(fede_su_t2009, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 2009, 0.53).
narrative_ontology:measurement_basis(fede_su_t2009, observed).
narrative_ontology:measurement(fede_su_t2014, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 2014, 0.56).
narrative_ontology:measurement_basis(fede_su_t2014, observed).
narrative_ontology:measurement(fede_su_t2019, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 2019, 0.56).
narrative_ontology:measurement_basis(fede_su_t2019, observed).
narrative_ontology:measurement(fede_su_t2025, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 2025, 0.55).
narrative_ontology:measurement_basis(fede_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_obligations__member_sovereignty_primary, resource_allocation).
narrative_ontology:affects_constraint(federation_membership_obligations__member_sovereignty_primary, federation_membership_obligations__integration_primary).
narrative_ontology:affects_constraint(federation_membership_obligations__member_sovereignty_primary, federation_membership_obligations__selective_solidarity).
narrative_ontology:affects_constraint(federation_membership_obligations__member_sovereignty_primary, eu_free_movement_worker_rules).

% DUAL FORMULATION NOTE:
% The colloquial label 'EU free movement and welfare' decomposes into three structurally distinct readings of the federation_membership_obligations kernel, per the ε-invariance principle: member_sovereignty_primary (this file — closure authority held by member state legislatures; ε authored 0.48 by its own lights), integration_primary (mobility constitutive; welfare boundaries yield to mobility rights; would author the same referent's ε near 0.75 with the victim set re-specified to the boundaries themselves), and selective_solidarity (contributory tiering; the beneficiary/victim line drawn by contribution history rather than nationality). All three readings share one referent — the standing closure regime — but author different ε over it; they are separate stories linked by affects_constraints, not one story with a measurement parameter. This reading is the upstream institutional holder of competence: national eligibility statutes are the vehicle through which selective_solidarity's tiering is enacted, and the object against which integration_primary litigates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
