% ============================================================================
% CONSTRAINT STORY: software_source_status__freedom_imperative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_source_status__freedom_imperative_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: software_source_status__freedom_imperative_reading
 *   human_readable: Proprietary Software Licensing as Ethical Injustice (Freedom-Imperative Reading)
 *   domain: software_engineering/political_economy/intellectual_property
 *
 * SUMMARY:
 *   This story instantiates the freedom-imperative reading of the
 *   software_source_status kernel: proprietary software licensing is not a
 *   neutral commercial arrangement but a categorical ethical wrong, and every
 *   instance of source concealment enters the victim set regardless of
 *   whether the arrangement is commercially voluntary. The referent of
 *   extractiveness is the standing arrangement (proprietary licensing as it
 *   actually operates) assessed by this reading's own lights — not the
 *   free-software alternative this reading endorses, which would trivially
 *   score near zero. Sibling readings (pragmatic development, property
 *   rights, utilitarian hybrid) are separate constraints with their own ε
 *   values and are not described here; see commentary.kernel_context and the
 *   omega variables for the committer structure linking them.
 *
 * KEY AGENTS:
 *   - proprietary_software_vendors: institutional beneficiary, sets licensing terms and captures rents from concealment
 *   - end_users_denied_source_access: powerless payer, denied inspection/repair/adaptation rights
 *   - downstream_developers_barred_from_modification: moderate-power payer, blocked from building on existing work
 *   - software_freedom_movement: organized payer/excluded voice, bears the cost of maintaining the alternative and is marginalized in procurement
 *   - copyright_enforcement_industry: institutional beneficiary, profits from enforcing the concealment regime
 *   - national_courts_and_legislatures: institutional observer/agenda_setter, launders the contested ethical claim into settled property law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__freedom_imperative_reading, 0.72).
domain_priors:suppression_score(software_source_status__freedom_imperative_reading, 0.68).
domain_priors:theater_ratio(software_source_status__freedom_imperative_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__freedom_imperative_reading, snare).
narrative_ontology:human_readable(software_source_status__freedom_imperative_reading, "Proprietary Software Licensing as Ethical Injustice (Freedom-Imperative Reading)").
narrative_ontology:topic_domain(software_source_status__freedom_imperative_reading, "software_engineering/political_economy/intellectual_property").

domain_priors:requires_active_enforcement(software_source_status__freedom_imperative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__freedom_imperative_reading, 'b76cc525-9e94-410b-a450-a3a887c38b44').
narrative_ontology:cs_kernel_codification('b76cc525-9e94-410b-a450-a3a887c38b44', distributed).
narrative_ontology:cs_authority_grounding('b76cc525-9e94-410b-a450-a3a887c38b44', distributed).
narrative_ontology:cs_reading_relation('b76cc525-9e94-410b-a450-a3a887c38b44', software_source_status__pragmatic_development_reading, influences).
narrative_ontology:cs_reading_relation('b76cc525-9e94-410b-a450-a3a887c38b44', software_source_status__property_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('b76cc525-9e94-410b-a450-a3a887c38b44', software_source_status__utilitarian_hybrid_reading, influences).
narrative_ontology:cs_axiom('b76cc525-9e94-410b-a450-a3a887c38b44', foundational, source_access_is_inalienable_right).
narrative_ontology:cs_axiom_status(source_access_is_inalienable_right, holdable).
narrative_ontology:cs_axiom_grounding('b76cc525-9e94-410b-a450-a3a887c38b44', source_access_is_inalienable_right, deontological).
narrative_ontology:cs_axiom('b76cc525-9e94-410b-a450-a3a887c38b44', foundational, restriction_of_the_four_freedoms_is_categorically_unjust).
narrative_ontology:cs_axiom_status(restriction_of_the_four_freedoms_is_categorically_unjust, holdable).
narrative_ontology:cs_axiom_grounding('b76cc525-9e94-410b-a450-a3a887c38b44', restriction_of_the_four_freedoms_is_categorically_unjust, deontological).
narrative_ontology:cs_reference_frame('b76cc525-9e94-410b-a450-a3a887c38b44', four_freedoms_founding_charter).
narrative_ontology:cs_drift_state('b76cc525-9e94-410b-a450-a3a887c38b44', platform_and_saas_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('b76cc525-9e94-410b-a450-a3a887c38b44', '').
narrative_ontology:cs_kernel_id(software_source_status__freedom_imperative_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__freedom_imperative_reading, proprietary_software_vendors).
narrative_ontology:constraint_beneficiary(software_source_status__freedom_imperative_reading, copyright_enforcement_industry).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, end_users_denied_source_access).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, downstream_developers_barred_from_modification).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, software_freedom_movement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Write and distribute binary-only software under EULAs that prohibit inspection, modification, and redistribution of source code. They set licensing terms, litigate against reverse engineering, and lobby for stronger copyright and DRM law. They capture recurring revenue from the restriction itself — the withheld source is the product's moat, not incidental to it.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, proprietary_software_vendors, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__freedom_imperative_reading, proprietary_software_vendors, beneficiary).

% Run software they cannot inspect, cannot verify for backdoors or defects, cannot repair when the vendor stops supporting it, and cannot adapt to their own needs. Under this reading they are denied something they are owed as a matter of basic autonomy over the machines that govern their lives; their only recourse is switching to a smaller pool of free-software alternatives, often at real functional cost.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, end_users_denied_source_access, payer,
    powerless, biographical, constrained, global).

% Would build on, fix, or extend existing software but are legally barred from doing so by copyright and license terms attached to source they cannot see. They must reinvent functionality that already exists, sign restrictive NDAs to get partial access, or abandon the improvement entirely. Under this reading, this waste is not a private contract outcome but a systemic injustice imposed on the commons of software knowledge.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, downstream_developers_barred_from_modification, payer,
    moderate, biographical, constrained, global).

% Advocates, licenses (GPL family), and free-software foundations that treat the four freedoms (run, study, share, modify) as inalienable rights. They bear the cost of maintaining parallel free alternatives, defending copyleft in court, and being marginalized in procurement and standards bodies that default to proprietary incumbents. They are structurally excluded from most enterprise and government purchasing decisions, which treat vendor lock-in as neutral rather than as the injustice this reading names it.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, software_freedom_movement, payer,
    organized, civilizational, mobile, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__freedom_imperative_reading, software_freedom_movement, excluded).

% Law firms, DRM vendors, and industry associations (e.g., anti-piracy bodies) whose business model depends on treating source concealment and copy restriction as legitimate and worth enforcing. They lobby for stronger enforcement (DMCA-style anti-circumvention law) and litigate against reverse-engineering and jailbreaking, which under this reading entrenches the underlying injustice rather than protecting a legitimate right.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, copyright_enforcement_industry, beneficiary,
    institutional, generational, arbitrage, global).

% Adjudicate copyright, patent, and anti-circumvention disputes and write the statutes (copyright term, DMCA-analog laws) that make source concealment enforceable. They treat the property-rights framing as the default legal baseline; under this reading, their neutrality is itself part of the injustice because it launders a contested ethical claim into settled law.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, national_courts_and_legislatures, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(software_source_status__freedom_imperative_reading, national_courts_and_legislatures, agenda_setter).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Proprietary licensing does coordinate something real: it lets a firm capture returns on software investment by excluding non-payers, which under mainstream economic reasoning funds development that might not otherwise occur. This reading acknowledges the coordination claim exists but denies it is legitimate coordination rather than dressed-up extraction.
% TRANSFER_FUNCTION: Moves control over inspection, modification, and redistribution of a piece of software from its users to its vendor, and moves the economic rents that flow from that control from the commons of potential collaborative improvement to the vendor's balance sheet.
% ABSENT_VOICES: Future users who inherit unmaintainable, unauditable software after a vendor exits the market; security researchers barred from responsible disclosure by anti-circumvention law; the broader public interest in a commons of inspectable, adaptable software. None of these are represented in the licensing negotiation itself.
% DISAPPEARANCE_RATIONALE: If proprietary licensing restrictions vanished overnight (source became universally open by default), the software industry's revenue model would restructure substantially toward services, support, and hosting; users would gain the ability to audit, fork, and repair software; and the enforcement apparatus built around copy-restriction (DRM, anti-circumvention litigation) would lose its object. This reading holds the rearrangement would be a correction, not a loss.
% FOUNDING_PROBLEM: Proprietary licensing was built to solve a real allocation problem: how to fund software development when copying is nearly free, by granting exclusive rights so a vendor can charge for scarcity it manufactures. The freedom-imperative reading holds this 'solution' was, from inception, an ethical wrong dressed as a funding mechanism — the founding problem it actually names is user subjugation to vendor control, which was never legitimate to begin with.
% FOUNDING_PROBLEM_CORROBORATION: Free Software Foundation founders and copyleft legal scholars attest the ethical-imperative framing from outside vendor interests, as does a body of security-research literature documenting harms from unauditable proprietary code (undisclosed vulnerabilities, undetectable backdoors). Proprietary vendors and mainstream IP law scholars dispute the injustice framing entirely and treat the funding-mechanism story as sufficient justification — so corroboration exists on both sides of the contest, which is why founding_problem_status is authored as contested rather than resolved.
narrative_ontology:disappearance_verdict(software_source_status__freedom_imperative_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__freedom_imperative_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__freedom_imperative_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(software_source_status__freedom_imperative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__freedom_imperative_reading, 0.72, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_source_status__freedom_imperative_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(software_source_status__freedom_imperative_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(software_source_status__freedom_imperative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.72) because, under this reading, every proprietary license transfers a right (inspection, modification, redistribution) that the reading holds is inalienable, to a vendor who did not need to withhold it to be compensated for labor. Suppression is authored moderately high (0.68) and rising over the interval because anti-circumvention law (DMCA-style statutes) and DRM have hardened over four decades, closing off self-help remedies (reverse engineering, jailbreaking) that once existed. Theater ratio is kept modest (0.28) because the enforcement apparatus is not mostly performative — litigation and DRM technically function to prevent the access this reading claims is owed.
 *
 * DIRECTIONALITY LOGIC:
 *   Vendors and the enforcement industry sit at the beneficiary end: they collect economically from the very act of withholding source, and their exit options (arbitrage — they can restructure licensing terms, relicense, or exit markets at will) reflect structural control rather than vulnerability. End users and downstream developers sit at the target end: they are structurally denied something this reading treats as a right, and their exit options are constrained by network effects, file-format lock-in, and the small size of the free-software alternative ecosystem. The software freedom movement is unusual: organized and mobile (able to build and use alternatives) but also a payer, because it bears the ongoing cost of maintaining a parallel commons against a much better-resourced proprietary sector.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding-problem interview is deliberately authored as contested rather than resolved: proprietary vendors and mainstream IP scholars maintain the funding-mechanism justification is still live and legitimate, while free-software advocates and security researchers hold that whatever funding problem existed was solved illegitimately from the start. This prevents the classification from mislabeling the vendor side's coordination claim (funding development) as pure fabrication, while still registering, from this reading's own lights, that the arrangement is an injustice rather than a neutral solution — the tangled coordination/extraction character other readings would find is deliberately NOT what this reading claims; this reading claims pure extraction dressed as a funding solution, hence the snare classification rather than tangled_rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_vs_contextual_software_rights,
    'Is source-code access a categorical, inalienable right analogous to other fundamental freedoms, or a contextual, negotiable interest whose value depends on the software''s function and stakes?',
    'No empirical resolution exists — this is fundamentally a normative/philosophical dispute about whether software freedom belongs in the category of inalienable rights (like due process) or in the category of negotiable commercial terms (like exclusivity clauses). Legal and philosophical argument, not data, would need to settle it, and even then would likely remain contested across ethical frameworks.',
    'If categorical, every proprietary license is a rights violation regardless of context, supporting the snare classification and the universal victim-set claim in this reading. If contextual, some proprietary arrangements (e.g. safety-critical embedded systems with strict liability, or short-lived niche tools) may be legitimate tangled-rope or even rope arrangements, and the universal victim-set claim would overreach.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(categorical_vs_contextual_software_rights, preference, 'Whether software freedom is a categorical right or a contextual interest — the load-bearing premise of this reading.').

omega_variable(
    kernel_committer_structure,
    'This constraint is one reading (freedom_imperative_reading) of the contested software_source_status kernel, which also has pragmatic_development_reading, property_rights_reading, and utilitarian_hybrid_reading as sibling constraints. Where exactly does the disagreement between readings live?',
    'The disagreement is located precisely at the moral status assigned to the act of withholding source: this reading treats withholding as a rights violation per se; property_rights_reading treats it as a legitimate exercise of ownership; pragmatic_development_reading brackets the rights question and asks only about development-quality outcomes; utilitarian_hybrid_reading asks only about aggregate welfare across contexts. No empirical study resolves this because the disagreement is over which normative framework governs the underlying fact pattern, which is common to all four readings.',
    'Adopting a different reading would change the victim set (property_rights_reading has essentially no victims — restriction is legitimate exercise of a right), change requires_active_enforcement''s moral valence (same enforcement mechanisms read as either injustice-maintenance or legitimate-right-protection), and would very likely flip the claimed_type away from snare toward rope (property_rights_reading) or tangled_rope/scaffold (utilitarian_hybrid_reading, which would weigh context).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_committer_structure, conceptual, 'Committer structure: naming the kernel, this reading, and where the sibling readings'' disagreement is structurally located.').

omega_variable(
    enforcement_apparatus_naturalization,
    'Has the legal enforcement apparatus around proprietary licensing (DMCA-style anti-circumvention law, DRM) become so normalized that it is now treated as a natural/default state rather than a contested policy choice, and does that normalization itself constitute part of the injustice this reading identifies?',
    'Historical and legal-genealogy analysis of when and how anti-circumvention statutes were passed, whose lobbying drove them, and whether courts and legislatures currently treat the property-rights baseline as requiring justification or as requiring no justification (the null hypothesis).',
    'If courts and legislatures treat proprietary-restriction-as-default as needing no justification, this corroborates the reading''s claim that the injustice has been laundered into apparently neutral law — supporting higher suppression and the observer/agenda_setter dual role for national_courts_and_legislatures. If the legal baseline is actively and openly contested in ongoing policy debate, the suppression metric may be overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_apparatus_naturalization, empirical, 'Whether legal normalization of restriction has entrenched the arrangement beyond what active contest would predict.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__freedom_imperative_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_source_status__freedom_imperative_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(soft_tr_t8, software_source_status__freedom_imperative_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(soft_tr_t16, software_source_status__freedom_imperative_reading, theater_ratio, 16, 0.21).
narrative_ontology:measurement(soft_tr_t24, software_source_status__freedom_imperative_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement(soft_tr_t32, software_source_status__freedom_imperative_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement(soft_tr_t40, software_source_status__freedom_imperative_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_source_status__freedom_imperative_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(soft_be_t8, software_source_status__freedom_imperative_reading, base_extractiveness, 8, 0.6).
narrative_ontology:measurement(soft_be_t16, software_source_status__freedom_imperative_reading, base_extractiveness, 16, 0.65).
narrative_ontology:measurement(soft_be_t24, software_source_status__freedom_imperative_reading, base_extractiveness, 24, 0.68).
narrative_ontology:measurement(soft_be_t32, software_source_status__freedom_imperative_reading, base_extractiveness, 32, 0.7).
narrative_ontology:measurement(soft_be_t40, software_source_status__freedom_imperative_reading, base_extractiveness, 40, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_source_status__freedom_imperative_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(soft_su_t8, software_source_status__freedom_imperative_reading, suppression_requirement, 8, 0.53).
narrative_ontology:measurement(soft_su_t16, software_source_status__freedom_imperative_reading, suppression_requirement, 16, 0.58).
narrative_ontology:measurement(soft_su_t24, software_source_status__freedom_imperative_reading, suppression_requirement, 24, 0.62).
narrative_ontology:measurement(soft_su_t32, software_source_status__freedom_imperative_reading, suppression_requirement, 32, 0.65).
narrative_ontology:measurement(soft_su_t40, software_source_status__freedom_imperative_reading, suppression_requirement, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__freedom_imperative_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(software_source_status__freedom_imperative_reading, 0.1).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, software_source_status__pragmatic_development_reading).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, software_source_status__property_rights_reading).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, software_source_status__utilitarian_hybrid_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling constraints decomposing the natural-language concept 'the software freedom debate' per the ε-invariance principle. Each reading of the software_source_status kernel produces a structurally distinct constraint with its own ε, its own beneficiary/victim structure, and its own claimed_type, evaluated over the SAME standing arrangement (proprietary software licensing) by each reading's own lights. This reading (freedom_imperative) claims snare with ε=0.72 and a universal victim set; property_rights_reading is expected to claim rope or mountain-adjacent with near-zero victims over the same arrangement; pragmatic_development_reading is expected to claim tangled_rope or scaffold graded on development-quality outcomes rather than rights; utilitarian_hybrid_reading is expected to claim tangled_rope with context-dependent victim/beneficiary weighting. All four should be linked bidirectionally via affects_constraints once generated.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
