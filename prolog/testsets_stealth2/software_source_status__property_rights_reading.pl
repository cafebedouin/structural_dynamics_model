% ============================================================================
% CONSTRAINT STORY: software_source_status__property_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_source_status__property_rights_reading, []).

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
 *   constraint_id: software_source_status__property_rights_reading
 *   human_readable: Proprietary Software Licensing Regime (Property-Rights Reading)
 *   domain: economic/legal/technological
 *
 * SUMMARY:
 *   The standing arrangement under contest: commercial software is
 *   distributed as closed binaries under restrictive licenses, source code is
 *   withheld, modification and redistribution are prohibited, and the whole
 *   structure rests on copyright statute, anti-circumvention law, technical
 *   protection measures, and click-through contracts. This file instantiates
 *   ONE reading of the software_source_status kernel, the
 *   property_rights_reading, under which that arrangement is largely a
 *   legitimate exercise of creator ownership and users hold contractual
 *   rights only. Per the epsilon referent rule, extractiveness is authored by
 *   this reading's own lights over the shared referent; the sibling readings,
 *   freedom_imperative, pragmatic_development, and utilitarian_hybrid, are
 *   separate constraints with their own epsilon values over the same
 *   referent, linked through network.affects_constraints. KEY AGENTS (by
 *   structural relationship): - proprietary_software_vendors: agenda-setting
 *   beneficiary (institutional/arbitrage) — drafts license terms, collects
 *   revenue - copyright_lawmaking_bodies: co-agenda-setter
 *   (institutional/arbitrage) — enacts and adjudicates the legal substrate -
 *   end_users_of_proprietary_software: primary target (powerless/constrained)
 *   — bears restrictions without negotiation - enterprise_software_buyers:
 *   dual payer/beneficiary (powerful/constrained) — pays premiums, receives
 *   accountability - independent_security_researchers: target
 *   (moderate/constrained) — bears legal exposure -
 *   independent_repair_technicians: target (moderate/constrained) — barred
 *   from servicing work - free_software_movement_participants: organized
 *   opposition bearing enforcement costs (organized/identity_locked) -
 *   competition_authorities: analytical observer (institutional/analytical).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__property_rights_reading, 0.2).
domain_priors:suppression_score(software_source_status__property_rights_reading, 0.7).
domain_priors:theater_ratio(software_source_status__property_rights_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__property_rights_reading, tangled_rope).
narrative_ontology:human_readable(software_source_status__property_rights_reading, "Proprietary Software Licensing Regime (Property-Rights Reading)").
narrative_ontology:topic_domain(software_source_status__property_rights_reading, "economic/legal/technological").

domain_priors:requires_active_enforcement(software_source_status__property_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__property_rights_reading, 'c230bf34-3eff-4c9c-8a62-f9cf44da8bbc').
narrative_ontology:cs_kernel_codification('c230bf34-3eff-4c9c-8a62-f9cf44da8bbc', fixed_text).
narrative_ontology:cs_authority_grounding('c230bf34-3eff-4c9c-8a62-f9cf44da8bbc', lineage).
narrative_ontology:cs_interpretation_layer_present('c230bf34-3eff-4c9c-8a62-f9cf44da8bbc').
narrative_ontology:cs_reading_relation('c230bf34-3eff-4c9c-8a62-f9cf44da8bbc', software_source_status__freedom_imperative_reading, forecloses).
narrative_ontology:cs_reading_relation('c230bf34-3eff-4c9c-8a62-f9cf44da8bbc', software_source_status__pragmatic_development_reading, influences).
narrative_ontology:cs_reading_relation('c230bf34-3eff-4c9c-8a62-f9cf44da8bbc', software_source_status__utilitarian_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('c230bf34-3eff-4c9c-8a62-f9cf44da8bbc', foundational, creator_exclusive_control_entitlement).
narrative_ontology:cs_axiom_status(creator_exclusive_control_entitlement, holdable).
narrative_ontology:cs_axiom_grounding('c230bf34-3eff-4c9c-8a62-f9cf44da8bbc', creator_exclusive_control_entitlement, deontological).
narrative_ontology:cs_axiom('c230bf34-3eff-4c9c-8a62-f9cf44da8bbc', foundational, users_hold_contractual_rights_only).
narrative_ontology:cs_axiom_status(users_hold_contractual_rights_only, holdable).
narrative_ontology:cs_axiom_grounding('c230bf34-3eff-4c9c-8a62-f9cf44da8bbc', users_hold_contractual_rights_only, conventional).
narrative_ontology:cs_reference_frame('c230bf34-3eff-4c9c-8a62-f9cf44da8bbc', creator_exclusive_control_baseline).
narrative_ontology:cs_drift_state('c230bf34-3eff-4c9c-8a62-f9cf44da8bbc', contemporary, gap(axiom_overriding, minor, false)).
narrative_ontology:cs_created_at('c230bf34-3eff-4c9c-8a62-f9cf44da8bbc', '2026-08-20T12:00:00Z').
narrative_ontology:cs_kernel_id(software_source_status__property_rights_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__property_rights_reading, proprietary_software_vendors).
narrative_ontology:constraint_victim(software_source_status__property_rights_reading, end_users_of_proprietary_software).
narrative_ontology:constraint_victim(software_source_status__property_rights_reading, independent_security_researchers).
narrative_ontology:constraint_victim(software_source_status__property_rights_reading, independent_repair_technicians).
narrative_ontology:constraint_victim(software_source_status__property_rights_reading, free_software_movement_participants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(software_source_status__property_rights_reading, enterprise_software_buyers).
narrative_ontology:constraint_victim(software_source_status__property_rights_reading, enterprise_software_buyers).
narrative_ontology:constraint_vindicates(software_source_status__property_rights_reading, copyright_incentive_doctrine).
narrative_ontology:constraint_vindicates(software_source_status__property_rights_reading, lockean_labor_property_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Publish software as compiled binaries under licenses they draft themselves. Withhold source code, prohibit modification and redistribution, embed technical protection measures, and operate legal teams that pursue infringement and reverse-engineering claims. Collect per-seat license fees and subscription revenue directly. Because they hold the copyrights, they can reprice, relicense, discontinue products, or convert purchase models at will; their exposure to the arrangement runs through market reputation and antitrust scrutiny rather than through any term they did not write.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, proprietary_software_vendors, agenda_setter,
    institutional, generational, arbitrage, global).

% Enact and amend the copyright statutes and anti-circumvention provisions that give private license terms their legal force, ratify international treaties harmonizing enforcement, and adjudicate disputes over what counts as infringement, fair use, or permissible research. Respond to lobbying from rights-holding industries and to counter-pressure from libraries, archives, consumer groups, and security professionals. Their commitments span election cycles and treaty obligations rather than any single product market.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, copyright_lawmaking_bodies, agenda_setter,
    institutional, generational, arbitrage, national).

% Purchase software at scale under negotiated enterprise agreements. Receive support commitments, indemnification, audit-ready documentation, and a liable counterparty, assurances the open-source channel often lacks. In exchange they accept license-count audits, usage telemetry, renewal leverage against sunk migration costs, and terms forbidding internal modification. Switching vendors after decade-long deployments costs more than renewing, which shapes their bargaining even when terms tighten.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, enterprise_software_buyers, payer,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__property_rights_reading, enterprise_software_buyers, beneficiary).

% Accept non-negotiated license terms as a condition of using software needed for work, school, and daily life. Cannot read or change the code they run, cannot legally bypass technical barriers even for interoperability or repair, and inherit vendor decisions about pricing, feature removal, and shutdown of online services their files depend on. Individual recourse is limited to choosing a different product where one exists or petitioning regulators collectively.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, end_users_of_proprietary_software, payer,
    powerless, biographical, constrained, global).

% Probe proprietary systems to find vulnerabilities before attackers do. Testing frequently requires circumventing technical protection measures, which exposes them to legal liability under anti-circumvention statutes regardless of intent. Publish under coordinated-disclosure timelines the vendor sets, or withhold findings. Some shift entirely to open-source targets where no legal barrier attaches.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, independent_security_researchers, payer,
    moderate, biographical, constrained, global).

% Service devices and machines running proprietary firmware and diagnostics. Manufacturers restrict access to diagnostic tools, pair parts to serialized components, and void warranties for unauthorized servicing. Right-to-repair statutes in some jurisdictions have reopened parts of this work; elsewhere the technicians operate under threat of legal action and lost dealer relationships.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, independent_repair_technicians, payer,
    moderate, biographical, constrained, regional).

% Develop and maintain openly licensed software and argue publicly that users should control their own computing. They are frequent targets of enforcement: takedown notices, infringement suits over license compliance, and exclusion from proprietary toolchains and certification programs. Their projects run on infrastructure and standards shaped by the proprietary ecosystem, and for many participants the commitment to openness is a defining identity, which makes retreat into proprietary work costly to self-conception.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, free_software_movement_participants, payer,
    organized, generational, identity_locked, global).

% Investigate bundling, refusal-to-license, and interoperability withholding in software markets. Take testimony from vendors, customers, and the open-source community, commission economic analysis of licensing practices, and can impose remedies such as mandated interfaces or behavioral commitments that would alter how license terms bind downstream actors.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, competition_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_source_status__property_rights_reading, proprietary_software_vendors).
narrative_ontology:fixing_cost_class(software_source_status__property_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns investment incentives for large-scale software production: exclusive rights assure developers and their financiers a recoverable return, enabling funded development of complex products; standardized license terms also reduce transaction costs in enterprise procurement and give buyers a single liable counterparty.
% TRANSFER_FUNCTION: Moves license revenue and subscription fees from software users, both consumers and enterprises, to rights-holding vendors; moves control over modification, redistribution, and repair from users to vendors; and places legal risk of circumvention and infringement onto security researchers, repairers, and competing developers.
% ABSENT_VOICES: Future developers who would learn by reading source have no seat in licensing design; end users in low-income regions priced by per-seat licensing are unrepresented; the eventual users of today's archived-but-locked file formats have no advocate at the table, though preservationists raise the point externally. Consumer advocates and library associations speak for some of these seats but hold no vote over license terms.
% DISAPPEARANCE_RATIONALE: If proprietary licensing and its enforcement machinery vanished overnight, vendor business models built on per-seat fees and subscriptions would collapse, funded development would reorganize around support contracts, foundations, and public grants, enterprises would lose their indemnification counterparties and rebuild procurement around escrow and insurance, and the repair and security-research fields would lose the legal barriers that currently define their boundaries. The software economy would rearrange around whatever appropriability mechanisms replaced exclusivity.
% FOUNDING_PROBLEM: Early software circulated freely alongside hardware; as software became a standalone product, developers faced appropriation without compensation, since copying undermined the ability to fund continued development. Copyright applied to software and restrictive licensing were built to secure a return on software labor.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting parties: empirical innovation economics documents that some appropriability mechanism materially affects investment in software-class goods, while disputing whether the current form is optimal; and open-source companies that adopted source-available licenses, MongoDB, Elastic, and HashiCorp among them, acted on the conviction that uncompensated reuse was starving their funding model, evidence of a live appropriation problem from parties who are not proprietary-restriction beneficiaries. Historians of the pre-commercial sharing era corroborate the genealogy. No corroborating source claims the problem is dead.
narrative_ontology:disappearance_verdict(software_source_status__property_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__property_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__property_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(software_source_status__property_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__property_rights_reading, 0.2, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_source_status__property_rights_reading_tests).
:- end_tests(software_source_status__property_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.20) because this reading assesses the arrangement by its own lights: license restrictions are, for this seat, legitimate exercises of ownership, and the residual 0.20 covers what even this reading concedes, subscription terms that take payment without conveying any durable right, terms imposed without negotiation, and enforcement aimed at interoperability competitors rather than pirates. Suppression is a raw structural property, unscaled by power or scope: 0.70 reflects the per-se legality of anti-circumvention, DRM, code signing, and the SaaS removal of any modifiable artifact, discounted because ecosystem-level exit into open-source alternatives remains available. Theater ratio 0.35: consent rituals around unread license agreements and artist-protection framing over publisher revenue are performative, while enforcement and funding flows underneath are real. Accessibility collapse 0.50: category-level alternatives persist, but within an entrenched ecosystem formats, switching costs, and certification paths collapse them. Resistance 0.70: a forty-year counter-regime, copyleft licensing, right-to-repair legislation, research-exemption advocacy, and sustained litigation defense. The measurement series run on one shared seven-point grid so every tracked metric is authored at every examined time point; the suppression_requirement series is authored because this story specifically traces an enforcement-capacity ratchet, with the 1998 anti-circumvention statute as the visible step. On the receipt surface: gains demonstrably accrue to the vendor seat, which collects the fees and controls the terms, so gain_flow names that seat; fixing is prohibitive for the lawmakers who could fix it, since dismantling software copyright would breach treaty obligations and reprice the asset base of the entire funded-software economy, a cost far exceeding any benefit to the fixer.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the vendor seat the arrangement is coordination it built and maintains, with extraction near the floor of legitimate return. From the end-user seat the same structure operates as non-negotiated restriction with no exit inside the product. The enterprise seat computes mixed: real accountability purchased at real premium, with renewal leverage doing the extracting. Researcher and repairer seats compute a legal-risk imposition that has little to do with the funding story at all. The engine derives these per-seat classifications from the structural data; the divergence between the vendor seat and the user seats is the measurable content of this story.
 *
 * DIRECTIONALITY LOGIC:
 *   Vendors sit nearest the beneficiary end: they collect the transfer, wrote the terms, and hold arbitrage-grade exit through relicensing and model shifts. Lawmakers co-administer and could restructure the constraint at will, placing them low on the target axis despite collecting nothing directly. End users derive high directionality from payer status plus constrained exit. Enterprises temper a high payer-derived directionality with the support and indemnity value they receive back, captured by the dual role. Researchers and repairers derive high directionality through legal exposure rather than fee payment. Free-software participants are an unusual seat: they are not consumption-side payers, but they bear concrete enforcement costs and are identity_locked into opposition, which traps them nearer the target end than their organizational power alone would predict. Global spatial scope raises verification difficulty, so the engine scales effective extraction upward modestly for the dispersed user seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem, securing a return on software labor against appropriation, is still live, corroborated by innovation economics and by the license reversals of open-source firms facing uncompensated cloud reuse, so this is not a resolved-mandatrophy case: status live paired with verdict world_rearranges is internally consistent and raises no zombie flag. The tangled_rope claim prevents mislabeling in both directions: a rope-only reading would erase the identifiable victims, the researchers prosecuted under anti-circumvention law, the repairers locked out, the users bound to terms they never read; a snare reading would erase the genuine funding coordination that even the arrangement's sharpest critics concede financed decades of software people rely on. Both the coordination function and the asymmetric extraction are real and ride the same structure, which is the tangled signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'Which reading of the software_source_status kernel governs a given jurisdiction or engineering culture, given that this story instantiates only the property_rights_reading?',
    'Track which sibling reading''s axioms dominate enacted statute, court doctrine, and procurement norms in a jurisdiction; the sibling stories carry their own epsilon values over the same referent.',
    'If a sibling reading gains governing status, the same standing arrangement is re-authored with a different epsilon and victim structure; classification flips at the kernel level, not the metric level.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'This constraint is one reading of the software_source_status kernel; sibling readings are separate constraints.').

omega_variable(
    epsilon_reading_index_divergence,
    'Epsilon here is authored low by the property-rights reading''s own lights over the fixed referent of the standing proprietary-licensing arrangement, while the freedom_imperative_reading authors high epsilon over the same referent; is the spread being read as data rather than reconciled away?',
    'Cross-read the sibling stories'' epsilon values against the identical referent; the spread locates the dispute structurally rather than empirically.',
    'Averaging the spread into one constraint would destroy the indexical signal; treating it as measurement maps exactly where and how strongly the readings diverge.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epsilon_reading_index_divergence, conceptual, 'Reading-indexed epsilon divergence over a shared referent is the datum, not an inconsistency.').

omega_variable(
    natural_right_vs_legislative_grant,
    'Does this reading ground the creator''s controlling right in a pre-political natural right, Lockean labor mixing, or in a legislative grant that legislatures may legitimately amend?',
    'Test adherents'' response to amendment proposals: if statutory revision is treated as morally authoritative recalibration the grounding is conventional; if statute is treated as merely recognizing a prior right the grounding is deontological.',
    'Determines the epistemic type of the foundational axiom and therefore how foreclosure against the freedom_imperative sibling computes; a purely conventional grounding softens the contradiction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_right_vs_legislative_grant, conceptual, 'Ambiguity between natural-right and legislative-grant grounding inside the property-rights reading itself.').

omega_variable(
    incentive_necessity_ambiguity,
    'Would large-scale funded software development persist at comparable scale without exclusive-rights assurance?',
    'Compare development output across regimes: pre-1980 sharing norms, weak-enforcement jurisdictions, and the funded open-source sector supported by foundations, support contracts, and public grants.',
    'If output persists robustly without exclusivity, the coordination function thins and the arrangement slides toward pure extraction; if not, the coordination function is load-bearing and the tangled reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incentive_necessity_ambiguity, empirical, 'Whether the coordination function attributed to exclusive rights is causally necessary or post hoc.').

omega_variable(
    subscription_quid_pro_quo_strain,
    'Has the shift from perpetual licenses to subscriptions severed the exchange this reading''s own justification requires, payment made without acquisition of any durable right?',
    'Compare post-termination access terms and renewal-price trajectories after discontinuation announcements against the reading''s own exchange logic of payment for acquired asset.',
    'If subscriptions break the internal justification, residual extractiveness rises even by this reading''s lights, pressuring recomputation upward without any sibling reading winning the argument.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subscription_quid_pro_quo_strain, empirical, 'Whether subscription conversion strains the property-rights reading''s internal justification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__property_rights_reading, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t1980, software_source_status__property_rights_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement_basis(soft_tr_t1980, observed).
narrative_ontology:measurement(soft_tr_t1990, software_source_status__property_rights_reading, theater_ratio, 1990, 0.18).
narrative_ontology:measurement_basis(soft_tr_t1990, observed).
narrative_ontology:measurement(soft_tr_t1998, software_source_status__property_rights_reading, theater_ratio, 1998, 0.22).
narrative_ontology:measurement_basis(soft_tr_t1998, observed).
narrative_ontology:measurement(soft_tr_t2003, software_source_status__property_rights_reading, theater_ratio, 2003, 0.26).
narrative_ontology:measurement_basis(soft_tr_t2003, observed).
narrative_ontology:measurement(soft_tr_t2010, software_source_status__property_rights_reading, theater_ratio, 2010, 0.3).
narrative_ontology:measurement_basis(soft_tr_t2010, observed).
narrative_ontology:measurement(soft_tr_t2017, software_source_status__property_rights_reading, theater_ratio, 2017, 0.33).
narrative_ontology:measurement_basis(soft_tr_t2017, observed).
narrative_ontology:measurement(soft_tr_t2025, software_source_status__property_rights_reading, theater_ratio, 2025, 0.35).
narrative_ontology:measurement_basis(soft_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(soft_be_t1980, software_source_status__property_rights_reading, base_extractiveness, 1980, 0.1).
narrative_ontology:measurement_basis(soft_be_t1980, observed).
narrative_ontology:measurement(soft_be_t1990, software_source_status__property_rights_reading, base_extractiveness, 1990, 0.13).
narrative_ontology:measurement_basis(soft_be_t1990, observed).
narrative_ontology:measurement(soft_be_t1998, software_source_status__property_rights_reading, base_extractiveness, 1998, 0.15).
narrative_ontology:measurement_basis(soft_be_t1998, observed).
narrative_ontology:measurement(soft_be_t2003, software_source_status__property_rights_reading, base_extractiveness, 2003, 0.17).
narrative_ontology:measurement_basis(soft_be_t2003, observed).
narrative_ontology:measurement(soft_be_t2010, software_source_status__property_rights_reading, base_extractiveness, 2010, 0.18).
narrative_ontology:measurement_basis(soft_be_t2010, observed).
narrative_ontology:measurement(soft_be_t2017, software_source_status__property_rights_reading, base_extractiveness, 2017, 0.19).
narrative_ontology:measurement_basis(soft_be_t2017, observed).
narrative_ontology:measurement(soft_be_t2025, software_source_status__property_rights_reading, base_extractiveness, 2025, 0.2).
narrative_ontology:measurement_basis(soft_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t1980, software_source_status__property_rights_reading, suppression_requirement, 1980, 0.2).
narrative_ontology:measurement_basis(soft_su_t1980, observed).
narrative_ontology:measurement(soft_su_t1990, software_source_status__property_rights_reading, suppression_requirement, 1990, 0.32).
narrative_ontology:measurement_basis(soft_su_t1990, observed).
narrative_ontology:measurement(soft_su_t1998, software_source_status__property_rights_reading, suppression_requirement, 1998, 0.48).
narrative_ontology:measurement_basis(soft_su_t1998, observed).
narrative_ontology:measurement(soft_su_t2003, software_source_status__property_rights_reading, suppression_requirement, 2003, 0.55).
narrative_ontology:measurement_basis(soft_su_t2003, observed).
narrative_ontology:measurement(soft_su_t2010, software_source_status__property_rights_reading, suppression_requirement, 2010, 0.62).
narrative_ontology:measurement_basis(soft_su_t2010, observed).
narrative_ontology:measurement(soft_su_t2017, software_source_status__property_rights_reading, suppression_requirement, 2017, 0.67).
narrative_ontology:measurement_basis(soft_su_t2017, observed).
narrative_ontology:measurement(soft_su_t2025, software_source_status__property_rights_reading, suppression_requirement, 2025, 0.7).
narrative_ontology:measurement_basis(soft_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__property_rights_reading, resource_allocation).
narrative_ontology:affects_constraint(software_source_status__property_rights_reading, software_source_status__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_source_status__property_rights_reading, software_source_status__pragmatic_development_reading).
narrative_ontology:affects_constraint(software_source_status__property_rights_reading, software_source_status__utilitarian_hybrid_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial debate over whether software should be open or proprietary decomposes, per the epsilon-invariance principle, into four structurally distinct constraints sharing the software_source_status kernel. Each reading authors its own epsilon over the same referent, the standing proprietary-licensing arrangement. This file instantiates the property_rights_reading (low reading-indexed epsilon, tangled_rope structural claim); the freedom_imperative sibling authors high epsilon over the identical referent. The upstream/downstream structure runs through legal machinery: this reading's statutory framework is the instrument the pragmatic_development sibling's copyleft tactics exploit, hence the influences edge.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
