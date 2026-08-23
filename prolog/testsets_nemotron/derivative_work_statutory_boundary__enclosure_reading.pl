% ============================================================================
% CONSTRAINT STORY: derivative_work_statutory_boundary__enclosure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_derivative_work_statutory_boundary__enclosure_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: derivative_work_statutory_boundary__enclosure_reading
 *   human_readable: Derivative Work Boundary — Enclosure Reading
 *   domain: intellectual_property/technology_governance/information_economics
 *
 * SUMMARY:
 *   The enclosure reading of the derivative work right treats any
 *   incorporation of copyrighted expression into a new work as preparing a
 *   derivative work requiring authorization. This reading emerged from the
 *   1976 Copyright Act's broad definition ('translation, musical arrangement,
 *   dramatization, fictionalization, motion picture version, sound recording,
 *   art reproduction, abridgment, condensation, or any other form in which a
 *   work may be recast, transformed, or adapted') and has been expanded
 *   through judicial interpretation (Bridgeport Music v. Dimension Films —
 *   'get a license or do not sample'; Oracle v. Google — API declaring code
 *   as protectable expression) and legislative reinforcement (DMCA 1201
 *   anti-circumvention, EU DSM Directive Article 17 upload filters). The
 *   constraint operates as a high-extraction snare: it requires active
 *   enforcement (automated filtering, litigation, licensing regimes),
 *   extracts from a broad class of downstream creators, and suppresses
 *   alternatives through pre-clearance requirements and statutory damages.
 *   The coordination function (bright-line administrability) is real but
 *   minimal compared to the extraction — the bright line is drawn at 'any
 *   use' rather than at commercially significant substitution.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_statutory_boundary__enclosure_reading, 0.82).
domain_priors:suppression_score(derivative_work_statutory_boundary__enclosure_reading, 0.78).
domain_priors:theater_ratio(derivative_work_statutory_boundary__enclosure_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__enclosure_reading, snare).
narrative_ontology:human_readable(derivative_work_statutory_boundary__enclosure_reading, "Derivative Work Boundary — Enclosure Reading").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__enclosure_reading, "intellectual_property/technology_governance/information_economics").

domain_priors:requires_active_enforcement(derivative_work_statutory_boundary__enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__enclosure_reading, 'cbc35b44-a00d-475c-b3f4-4d56e17d6688').
narrative_ontology:cs_kernel_codification('cbc35b44-a00d-475c-b3f4-4d56e17d6688', formalized).
narrative_ontology:cs_authority_grounding('cbc35b44-a00d-475c-b3f4-4d56e17d6688', lineage).
narrative_ontology:cs_interpretation_layer_present('cbc35b44-a00d-475c-b3f4-4d56e17d6688').
narrative_ontology:cs_reading_relation('cbc35b44-a00d-475c-b3f4-4d56e17d6688', derivative_work_statutory_boundary__coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('cbc35b44-a00d-475c-b3f4-4d56e17d6688', derivative_work_statutory_boundary__hybrid_carveout_reading, influences).
narrative_ontology:cs_axiom('cbc35b44-a00d-475c-b3f4-4d56e17d6688', foundational, any_incorporation_requires_authorization).
narrative_ontology:cs_axiom_status(any_incorporation_requires_authorization, holdable).
narrative_ontology:cs_axiom_grounding('cbc35b44-a00d-475c-b3f4-4d56e17d6688', any_incorporation_requires_authorization, conventional).
narrative_ontology:cs_axiom('cbc35b44-a00d-475c-b3f4-4d56e17d6688', foundational, derivative_right_as_property_entitlement).
narrative_ontology:cs_axiom_status(derivative_right_as_property_entitlement, holdable).
narrative_ontology:cs_axiom_grounding('cbc35b44-a00d-475c-b3f4-4d56e17d6688', derivative_right_as_property_entitlement, deontological).
narrative_ontology:cs_reference_frame('cbc35b44-a00d-475c-b3f4-4d56e17d6688', statutory_derivative_work_definition_1976).
narrative_ontology:cs_drift_state('cbc35b44-a00d-475c-b3f4-4d56e17d6688', contemporary_platform_enforcement_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('cbc35b44-a00d-475c-b3f4-4d56e17d6688', '').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__enclosure_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__enclosure_reading, incumbent_rights_holders).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__enclosure_reading, collective_management_organizations).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__enclosure_reading, licensing_platforms).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, independent_creators).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, transformative_artists).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, software_developers).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, researchers_data_miners).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, educational_institutions).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, archivists_preservationists).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__enclosure_reading, copyright_as_property_absolutism).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__enclosure_reading, derivative_work_control_as_incentive_necessity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Major media conglomerates, publishers, and studio libraries hold vast catalogs of copyrighted works. They enforce the enclosure reading through automated content identification systems, litigation threats, and licensing regimes that require payment for any incorporation of protected expression. They capture the licensing revenue stream and control the gatekeeping infrastructure.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, incumbent_rights_holders, beneficiary,
    institutional, generational, arbitrage, global).

% Organizations like ASCAP, BMI, Harry Fox Agency, and international equivalents administer blanket licensing, collect royalties, and lobby for expansive derivative work definitions. They take administrative fees from every license and set the practical terms under which creators can legally build on existing works.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, collective_management_organizations, beneficiary,
    organized, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(derivative_work_statutory_boundary__enclosure_reading, collective_management_organizations, agenda_setter).

% Digital platforms (Content ID, Audible Magic, Pex, etc.) provide the technical enforcement layer — automated fingerprinting, takedown, and monetization redirection. They extract platform fees from both rights holders and creators, and their business model depends on the enclosure reading's maximalist scope.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, licensing_platforms, beneficiary,
    organized, biographical, mobile, global).

% Individual artists, musicians, writers, and video makers who incorporate existing expression (sampling, quotation, remix, fan works, parody). They face licensing fees they cannot afford, takedown strikes that destroy channels, and legal risk that chills creation. Exit means abandoning their artistic practice or moving to jurisdictions with narrower enforcement — often impractical for global platforms.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, independent_creators, payer,
    powerless, biographical, constrained, global).

% Artists whose practice is fundamentally transformative (collage, appropriation art, remix culture, hip-hop sampling, documentary filmmakers). The enclosure reading treats their core method as presumptive infringement. They either license at monopoly rates, create in legal gray zones, or self-censor. Fair use defenses exist but are expensive to litigate and unpredictable.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, transformative_artists, payer,
    moderate, biographical, constrained, global).

% Developers building tools that process copyrighted works (text mining, ML training, format shifting, interoperability layers, API clients). The enclosure reading treats intermediate copying in development as derivative work preparation. They face licensing demands for ephemeral technical copies, API restrictions, and anti-circumvention barriers. Open source alternatives exist but cannot replicate proprietary training data or platform ecosystems.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, software_developers, payer,
    moderate, biographical, constrained, global).

% Academic and commercial researchers conducting text and data mining, computational analysis, and corpus linguistics on copyrighted corpora. The enclosure reading requires licensing for research copies even when outputs contain no protected expression. Institutional licenses are expensive, restrictive, and often unavailable for cross-border research.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, researchers_data_miners, payer,
    moderate, biographical, constrained, global).

% Schools, universities, and libraries creating course materials, digital archives, and accessible format copies. The enclosure reading forces them to pay for uses that would be fair use under other readings — coursepacks, lecture recordings, distance learning, accessibility conversions. Budget-constrained institutions either pay monopoly rates or reduce offerings.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, educational_institutions, payer,
    organized, generational, constrained, national).

% Memory institutions preserving born-digital and deteriorating analog works. Format migration, emulation, and web archiving require intermediate copying that the enclosure reading treats as derivative work preparation. They operate on public/non-profit budgets with no licensing budget for orphan works or mass-digitization. Exit is not an option — preservation is their mandate.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, archivists_preservationists, payer,
    moderate, generational, trapped, global).

% Judicial and legislative bodies that define the derivative work boundary through precedent and statute. Under the enclosure reading, they have expanded the definition through cases like Bridgeport (sampling), Oracle v. Google (API structure), and legislative expansions (DMCA 1201, EU DSM Directive Art. 17). They set the enforcement parameters but face lobbying pressure from beneficiary groups.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, courts_legislatures, agenda_setter,
    institutional, generational, analytical, national).

% Civil society organizations (EFF, Creative Commons, Communia, library associations) arguing for narrower derivative work boundaries, robust fair use, and public domain protection. They are structurally excluded from the licensing negotiation table — their constituency (the public) has no concentrated economic interest to bring to the bargaining process.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, public_domain_advocates, excluded,
    organized, civilizational, trapped, global).

% Legal scholars and economists analyzing the derivative work boundary's effects on innovation, culture, and welfare. They observe the enclosure reading's expansion and document its chilling effects, but their analysis carries no enforcement power. They provide the evidentiary record for future reform.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, ip_scholars_analytical, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, administrable rule for when permission is needed to build on existing expression — in theory reducing transaction costs by establishing a bright-line boundary rather than case-by-case fair use analysis.
% TRANSFER_FUNCTION: Moves licensing revenue and control over follow-on creation from downstream creators (independent artists, developers, researchers, educators, archivists) to upstream rights holders (media conglomerates, collective management organizations, licensing platforms) through mandatory pre-clearance for any incorporation of protected expression.
% ABSENT_VOICES: The public domain itself — future generations who inherit a thinned cultural commons; orphan work holders who cannot be located to license; users in jurisdictions without fair use/fair dealing equivalents who have no defense at all; non-commercial creators whose practice is not organized as an economic interest group.
% DISAPPEARANCE_RATIONALE: If the enclosure reading vanished overnight, the derivative work boundary would revert to a narrower, transformative-use-centered standard. Downstream creators would immediately resume sampling, remixing, text mining, format shifting, and archival copying without licensing. Rights holders would lose the licensing revenue stream from these uses. Collective management organizations would lose administrative fees. Licensing platforms would lose their core enforcement product. The creative economy would reorganize around permissionless innovation with post-hoc compensation models.
% FOUNDING_PROBLEM: Early copyright statutes needed a workable boundary to distinguish authorized adaptations (translations, abridgments, dramatizations) from new original works, in an era when copying required industrial printing presses and distribution networks.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem — distinguishing industrial-scale adaptations from original works in an analog reproduction economy — is dead. Digital technology makes every use a 'copy' and every creator a potential adapter. The enclosure reading's expansion to cover non-commercial, transformative, intermediate, and research uses is acknowledged by the U.S. Copyright Office (Section 512 study, 2017), the EU Commission (DSM Directive impact assessment), and independent economic analyses (Hargreaves Review 2011, Bently & Kretschmer 2019) as a departure from the original statutory purpose. The beneficiary groups (incumbent rights holders, CMOs) self-assert the problem remains live; no corroborating source outside the beneficiary set agrees.
narrative_ontology:disappearance_verdict(derivative_work_statutory_boundary__enclosure_reading, world_rearranges).
narrative_ontology:founding_problem_status(derivative_work_statutory_boundary__enclosure_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(derivative_work_statutory_boundary__enclosure_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(derivative_work_statutory_boundary__enclosure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(derivative_work_statutory_boundary__enclosure_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(derivative_work_statutory_boundary__enclosure_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(derivative_work_statutory_boundary__enclosure_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(derivative_work_statutory_boundary__enclosure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.82) reflects the near-total capture of follow-on creation value by upstream rights holders through mandatory licensing. Suppression (0.78) reflects the combination of automated content ID, statutory damages up to $150k/work, injunctive relief, and anti-circumvention rules that make technical workarounds illegal. Theater ratio (0.25) is low — the enforcement machinery is functional and the licensing revenue is real, not performative. Accessibility collapse (0.42) is moderate: fair use and specific exceptions (library, education, temporary copies) provide partial but unreliable exits. Resistance (0.68) is substantial: fair use litigation, open culture movements, legislative reform attempts (Copyright Office Section 512 study, MUSIC Act, CASE Act critiques), and jurisdictional arbitrage (server location, platform policy) all push back.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (incumbent rights holders, CMOs, platforms), the enclosure reading appears as necessary property protection enabling efficient licensing markets — a coordination solution. From the payer seats (creators, developers, researchers, educators, archivists), the same structure operates as a permission culture that extracts monopoly rents for uses that do not substitute for the original — a snare. The engine computes this divergence from the structural power/exit asymmetry: beneficiaries hold institutional power and arbitrage exit; payers hold little power and constrained/trapped exit. The agenda_setter seat (courts/legislatures) experiences the constraint as a policy calibration problem — but their calibration is structurally biased toward beneficiary interests because beneficiaries concentrate lobbying resources while payers' costs are diffuse.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent rights holders, CMOs, and licensing platforms are structural beneficiaries (d ≈ 0.1–0.2) — they collect licensing revenue, control enforcement infrastructure, and face arbitrage-grade exit (global portfolio diversification, political capture). Downstream creators (independent artists, transformative artists, developers, researchers, educators, archivists) are structural targets (d ≈ 0.7–0.9) — they pay licensing fees or bear litigation risk, face constrained or trapped exit (platform dependence, mandate-bound preservation, jurisdictional lock-in), and lack collective bargaining power. Courts/legislatures sit near symmetric (d ≈ 0.5) — they administer the system but capture no direct revenue. Public domain advocates are excluded (d undefined — they are not seated at the constraint's table). Analytical observers sit at d = 0.5 by definition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (distinguishing industrial adaptations in an analog economy) is dead. The enclosure reading persists because it generates concentrated revenue for beneficiaries who invest in its maintenance (lobbying, litigation, automation), while the costs are distributed across millions of downstream creators who cannot organize equivalent resistance. The constraint is not coordination that outlived its purpose — it is extraction that was never coordination, wrapped in the language of the original statutory purpose. Mandatrophy is unresolved: the arrangement's mandate (promote progress by incentivizing creation) has been inverted (suppress follow-on creation to protect incumbent revenue), but the inversion is denied by the authority structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enclosure_reading_naturalness,
    'Is the enclosure reading a natural interpretation of the statutory text, or a constructed expansion that benefits identifiable incumbents?',
    'Legislative history analysis of the 1976 Act''s ''any other form'' language; comparative analysis of pre-1976 case law on derivative works; economic analysis of who captures value from the expansion.',
    'If the enclosure reading is a constructed expansion (not compelled by text), the constraint is a false summit candidate — it presents as statutory mandate but operates as incumbent rent-seeking. The false_summit_mountain signature would trigger if the constraint were claimed as mountain; as a claimed snare, the omega documents the illegitimacy of the statutory cover story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enclosure_reading_naturalness, conceptual, 'Whether the enclosure reading''s statutory cover story is genuine or constructed.').

omega_variable(
    fair_use_as_safety_valve_effectiveness,
    'Does fair use/fair dealing function as an effective safety valve for the enclosure reading''s overreach, or is it too uncertain and expensive to protect most downstream creators?',
    'Empirical study of fair use litigation outcomes for transformative uses; survey of creator self-censorship behavior; analysis of insurance availability for fair use defenses.',
    'If fair use is ineffective for most creators, the enclosure reading''s extraction is near-total for the payer class — accessibility_collapse is higher than measured. If fair use works, the constraint is a tangled_rope (coordination + extraction) rather than a pure snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fair_use_as_safety_valve_effectiveness, empirical, 'Whether the doctrinal safety valve actually limits extraction in practice.').

omega_variable(
    automated_enforcement_error_rate,
    'What is the false positive rate of automated content identification systems, and how does it affect creators who are not infringing?',
    'Platform transparency reports (YouTube Content ID, Meta Rights Manager); independent auditing of fingerprinting accuracy; analysis of counternotification and appeal outcomes.',
    'High false positive rates mean the enclosure reading suppresses non-infringing uses (public domain, fair use, licensed) — suppression is higher than the legal rule alone indicates. This strengthens the snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(automated_enforcement_error_rate, empirical, 'Technical enforcement overreach beyond the legal boundary.').

omega_variable(
    orphan_works_scale,
    'What fraction of the cultural record is orphan works (copyrighted but rights holder unlocatable), and how does the enclosure reading affect their use?',
    'Library of Congress and European Commission orphan works studies; mass-digitization project clearance logs; analysis of works unavailable for licensing.',
    'If orphan works are a large fraction, the enclosure reading creates a de facto prohibition on using most 20th-century culture — extraction extends to works no beneficiary even claims. This is pure deadweight loss, strengthening the snare classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(orphan_works_scale, empirical, 'Scale of the enclosure reading''s deadweight loss from orphan works.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__enclosure_reading, 1976, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deri_tr_t1976, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 1976, 0.1).
narrative_ontology:measurement(deri_tr_t1988, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 1988, 0.12).
narrative_ontology:measurement(deri_tr_t1998, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 1998, 0.15).
narrative_ontology:measurement(deri_tr_t2001, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 2001, 0.18).
narrative_ontology:measurement(deri_tr_t2005, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 2005, 0.2).
narrative_ontology:measurement(deri_tr_t2010, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 2010, 0.22).
narrative_ontology:measurement(deri_tr_t2015, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 2015, 0.23).
narrative_ontology:measurement(deri_tr_t2020, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 2020, 0.24).
narrative_ontology:measurement(deri_tr_t2026, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 2026, 0.25).

% Extraction over time
narrative_ontology:measurement(deri_be_t1976, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 1976, 0.35).
narrative_ontology:measurement(deri_be_t1988, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 1988, 0.42).
narrative_ontology:measurement(deri_be_t1998, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 1998, 0.58).
narrative_ontology:measurement(deri_be_t2001, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 2001, 0.63).
narrative_ontology:measurement(deri_be_t2005, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 2005, 0.68).
narrative_ontology:measurement(deri_be_t2010, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 2010, 0.72).
narrative_ontology:measurement(deri_be_t2015, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 2015, 0.76).
narrative_ontology:measurement(deri_be_t2020, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 2020, 0.8).
narrative_ontology:measurement(deri_be_t2026, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 2026, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(deri_su_t1976, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 1976, 0.25).
narrative_ontology:measurement(deri_su_t1988, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 1988, 0.35).
narrative_ontology:measurement(deri_su_t1998, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 1998, 0.55).
narrative_ontology:measurement(deri_su_t2001, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 2001, 0.62).
narrative_ontology:measurement(deri_su_t2005, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 2005, 0.68).
narrative_ontology:measurement(deri_su_t2010, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 2010, 0.71).
narrative_ontology:measurement(deri_su_t2015, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 2015, 0.74).
narrative_ontology:measurement(deri_su_t2020, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 2020, 0.76).
narrative_ontology:measurement(deri_su_t2026, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 2026, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_work_statutory_boundary__enclosure_reading, information_standard).
narrative_ontology:boltzmann_floor_override(derivative_work_statutory_boundary__enclosure_reading, 0.03).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__enclosure_reading, fair_use_doctrine).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__enclosure_reading, dmca_1201_anticircumvention).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__enclosure_reading, eu_dsm_directive_article_17).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__enclosure_reading, collective_management_monopoly).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__enclosure_reading, orphan_works_deadlock).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__enclosure_reading, ai_training_copyright).

% DUAL FORMULATION NOTE:
% This constraint is one reading (enclosure_reading) of the derivative_work_statutory_boundary kernel. The coordination_reading and hybrid_carveout_reading are sibling constraints with substantially lower extractiveness. The kernel's ε-invariance is violated by the label 'derivative work right' — the three readings instantiate three different constraints with different ε, beneficiaries, and victims. They are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(derivative_work_statutory_boundary__enclosure_reading, organized, 0.15).
constraint_indexing:directionality_override(derivative_work_statutory_boundary__enclosure_reading, powerless, 0.88).
constraint_indexing:directionality_override(derivative_work_statutory_boundary__enclosure_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
