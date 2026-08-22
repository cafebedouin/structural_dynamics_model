% ============================================================================
% CONSTRAINT STORY: copyright_constitutional_mandate__public_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_copyright_constitutional_mandate__public_scaffold_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: copyright_constitutional_mandate__public_scaffold_reading
 *   human_readable: Copyright Constitutional Mandate — Public Scaffold Reading
 *   domain: intellectual_property/constitutional/political_economy
 *
 * SUMMARY:
 *   This constraint instantiates the public scaffold reading of the Copyright
 *   Clause (Art. I, §8, cl. 8): 'To promote the Progress of Science and
 *   useful Arts, by securing for limited Times to Authors and Inventors the
 *   exclusive Right to their respective Writings and Discoveries.' Under this
 *   reading, the monopoly grant is a temporary coordination mechanism — a
 *   scaffold — whose sole constitutional justification is public domain
 *   enrichment. The constraint's extractiveness is structurally
 *   low-to-moderate (ε=0.22 at interval end) because the monopoly is means,
 *   not end; fair use, term limits, and anti-enclosure norms are not
 *   exceptions but the operating logic. The measured theater ratio rises
 *   mid-interval (1998 CTEA era) when term extensions decoupled from
 *   incentive evidence, then falls as anti-enclosure movements and fair use
 *   expansion reassert the scaffold logic. The constraint requires active
 *   enforcement (registration, deposit, notice formalities historically; DMCA
 *   safe harbors now) but its enforcement target is free-riding on the
 *   coordination mechanism, not extraction from the public.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_constitutional_mandate__public_scaffold_reading, 0.22).
domain_priors:suppression_score(copyright_constitutional_mandate__public_scaffold_reading, 0.18).
domain_priors:theater_ratio(copyright_constitutional_mandate__public_scaffold_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_constitutional_mandate__public_scaffold_reading, scaffold).
narrative_ontology:human_readable(copyright_constitutional_mandate__public_scaffold_reading, "Copyright Constitutional Mandate — Public Scaffold Reading").
narrative_ontology:topic_domain(copyright_constitutional_mandate__public_scaffold_reading, "intellectual_property/constitutional/political_economy").

domain_priors:requires_active_enforcement(copyright_constitutional_mandate__public_scaffold_reading).
narrative_ontology:has_sunset_clause(copyright_constitutional_mandate__public_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__public_scaffold_reading, '03c26c78-99f4-41b2-b802-0b8541e66d6a').
narrative_ontology:cs_kernel_codification('03c26c78-99f4-41b2-b802-0b8541e66d6a', fixed_text).
narrative_ontology:cs_authority_grounding('03c26c78-99f4-41b2-b802-0b8541e66d6a', lineage).
narrative_ontology:cs_interpretation_layer_present('03c26c78-99f4-41b2-b802-0b8541e66d6a').
narrative_ontology:cs_reading_relation('03c26c78-99f4-41b2-b802-0b8541e66d6a', copyright_constitutional_mandate__corporate_enclosure_reading, forecloses).
narrative_ontology:cs_reading_relation('03c26c78-99f4-41b2-b802-0b8541e66d6a', copyright_constitutional_mandate__judicial_ambiguity_reading, coexists_with).
narrative_ontology:cs_axiom('03c26c78-99f4-41b2-b802-0b8541e66d6a', foundational, monopoly_as_scaffold_for_public_domain).
narrative_ontology:cs_axiom_status(monopoly_as_scaffold_for_public_domain, holdable).
narrative_ontology:cs_axiom_grounding('03c26c78-99f4-41b2-b802-0b8541e66d6a', monopoly_as_scaffold_for_public_domain, conventional).
narrative_ontology:cs_axiom('03c26c78-99f4-41b2-b802-0b8541e66d6a', foundational, limited_times_means_meaningful_limit).
narrative_ontology:cs_axiom_status(limited_times_means_meaningful_limit, holdable).
narrative_ontology:cs_axiom_grounding('03c26c78-99f4-41b2-b802-0b8541e66d6a', limited_times_means_meaningful_limit, conventional).
narrative_ontology:cs_reference_frame('03c26c78-99f4-41b2-b802-0b8541e66d6a', founding_era_scaffold_calibration).
narrative_ontology:cs_drift_state('03c26c78-99f4-41b2-b802-0b8541e66d6a', post_ctea_fair_use_expansion, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('03c26c78-99f4-41b2-b802-0b8541e66d6a', '').
narrative_ontology:cs_kernel_id(copyright_constitutional_mandate__public_scaffold_reading, copyright_constitutional_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, the_public_domain).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, creators_spectrum).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__public_scaffold_reading, copyright_clause_purpose).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__public_scaffold_reading, limited_times_doctrine).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__public_scaffold_reading, public_domain_enrichment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives every work upon term expiration as a matter of constitutional design. Also benefits continuously from fair use, idea/expression dichotomy, and other public-domain-preserving doctrines that operate within the scaffold. Exit is arbitrage-grade: the public domain cannot be exited from; it is the baseline state. Its power is organized through libraries, archives, creative commons, and public interest litigation.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, the_public_domain, beneficiary,
    organized, generational, arbitrage, national).

% Receive a time-limited monopoly calibrated to the incentive threshold for creation. This includes individual authors, artists, musicians, and small-scale creators. Their exit is mobile: they can choose not to create, or create under open licenses, or exit copyright entirely via CC0/public domain dedication. They benefit from the coordination mechanism (legal clarity, enforcement against verbatim copying) but do not capture the constraint's gains — the gains flow to the public domain.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, creators_spectrum, beneficiary,
    moderate, biographical, mobile, national).

% Hold large portfolios of copyrighted works acquired through work-for-hire and assignment. They lobby for term extension, scope expansion, and enforcement intensification — attempting to mutate the constraint from scaffold to enclosure. Under this reading, they are not beneficiaries of the constraint as constitutionally designed; they are agents of a competing reading. Their exit is constrained: they cannot easily abandon their portfolios, but they can (and do) push for legal change.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, corporate_rightsholders, excluded,
    institutional, generational, constrained, global).

% Administer the public domain's intake (deposit, preservation, access) and advocate for term limits, fair use, orphan works reform, and digital access. They set the agenda for the scaffold's maintenance. Their power is organized through professional associations (ALA, ARL) and statutory mandates (Section 108, deposit requirements).
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, libraries_archives, agenda_setter,
    organized, generational, analytical, national).

% Holds the constitutional power to define 'limited Times' and 'exclusive Right.' Under this reading, Congress's role is to calibrate the scaffold — setting terms at the incentive threshold, not beyond. Congress also observes the constraint's operation through oversight and Copyright Office reports. Its exit is analytical: it can change the law but is constitutionally bound to the promotion-of-progress purpose.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, congress, agenda_setter,
    institutional, biographical, analytical, national).
narrative_ontology:stakeholder_secondary_role(copyright_constitutional_mandate__public_scaffold_reading, congress, observer).

% Adjudicate the boundary between scaffold and enclosure: term limits (Eldred), fair use (Campbell, Google v. Oracle), first sale (Kirtsaeng), public domain restoration (Golan). Their role is analytical — interpreting whether the constraint remains a scaffold or has become an enclosure. They have no exit; they are the interpretive structure.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the public goods problem for creative works: without a temporary monopoly, creators cannot recoup fixed costs of creation in a world where copies are cheap; the monopoly enables a market that funds creation, and its expiration returns the work to the public domain for unrestricted use.
% TRANSFER_FUNCTION: Moves a time-limited exclusive right from the public domain to creators, enabling them to charge for copies/licenses. The transfer is calibrated: the right's scope and duration are set at the minimum needed to incentivize creation, not at the maximum the market will bear. Gains flow to the public domain upon expiration (and continuously via fair use).
% ABSENT_VOICES: Future generations who inherit the public domain (or its depletion) are structurally excluded from the legislative calculus that sets terms. Orphan work creators — whose works are locked by term but unexploited by rightsholders — are excluded from the enforcement calculus. Both would object to term extension without incentive justification.
% DISAPPEARANCE_RATIONALE: If the constitutional mandate vanished overnight, the statutory copyright regime would lose its constitutional mooring. Congress could still enact copyright, but without the 'promote Progress' purpose limitation, the constraint would likely mutate toward the corporate enclosure reading — perpetual terms, minimal fair use, maximal scope. The public domain would stop growing. The world rearranges because the scaffold's sunset logic is constitutionally grounded.
% FOUNDING_PROBLEM: In 1790, the founding problem was incentivizing creation and distribution of books, maps, and charts in a new nation with weak markets and expensive reproduction. The scaffold (14+14 year terms, registration, notice, deposit) was calibrated to that problem.
% FOUNDING_PROBLEM_CORROBORATION: The Copyright Office and major rightsholder associations attest the problem is still live (piracy, digital reproduction). Libraries, scholars (Litman, Samuelson, Boyle, Lessig), and the Supreme Court in Feist and Golan (dissents) attest the problem has mutated: digital reproduction lowers the incentive threshold, but terms have extended. No single authoritative corroboration exists — the contest is the point.
narrative_ontology:disappearance_verdict(copyright_constitutional_mandate__public_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(copyright_constitutional_mandate__public_scaffold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(copyright_constitutional_mandate__public_scaffold_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(copyright_constitutional_mandate__public_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(copyright_constitutional_mandate__public_scaffold_reading, 0.22, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(copyright_constitutional_mandate__public_scaffold_reading_tests).
:- end_tests(copyright_constitutional_mandate__public_scaffold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is low because the monopoly's scope and duration are calibrated to the incentive threshold, not to rent maximization. Suppression is low because alternatives (public domain, fair use, commons-based production) remain legally and practically accessible. Theater ratio tracks the historical divergence between statutory terms and incentive evidence, peaking at the CTEA/Sonny Bono extension (1998) and declining with Golan, Kirtsaeng, and the fair use expansion post-Campbell. Accessibility collapse is moderate (0.35) — the public domain is legally accessible but practically thinned by orphan works and term length; resistance (0.45) reflects sustained scholarly, judicial, and legislative pushback against enclosure. The scaffold claim is structurally true because the constraint carries a sunset logic: each work's term expiration is a designed reversion to the public domain.
 *
 * DIRECTIONALITY LOGIC:
 *   The public domain is the primary beneficiary (d ≈ 0.05): the constraint's entire justification is its enrichment. Creators are secondary beneficiaries (d ≈ 0.3): they receive a time-limited monopoly calibrated to incentive needs, not a property right. No victim seat exists under this reading — the constraint is a coordination regime, not an extraction mechanism. Corporate rightsholders who lobby for term extension are not victims of this constraint; they are agents attempting to mutate it into a different constraint (the corporate_enclosure_reading). The engine's directionality derivation from beneficiary declarations + exit options will correctly place the public domain at the subsidy end and creators near symmetric.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy question is live: the founding problem (incentivizing creation in a pre-digital reproduction economy) has mutated. Digital near-zero marginal cost reproduction changes the incentive threshold. This reading holds the constraint remains a scaffold because the public domain enrichment function is still the constitutional north star; the corporate enclosure reading argues the scaffold has become a snare. The engine's per-seat computation will reveal whether the payer seat (if any) experiences extraction — under this reading, there is none.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Where does the public scaffold reading end and the corporate enclosure reading begin in actual judicial doctrine?',
    'Track Supreme Court language in Eldred, Golan, and subsequent term/fair use cases: does the Court treat ''limited times'' as a meaningful constraint (scaffold) or as ''whatever Congress says'' (enclosure)?',
    'If the Court treats ''limited times'' as non-justiciable, the scaffold reading''s ε rises toward enclosure territory; if the Court enforces a meaningful limit, the scaffold reading maintains low ε.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether judicial doctrine forecloses the scaffold reading or leaves it live.').

omega_variable(
    digital_incentive_threshold,
    'Does digital reproduction''s near-zero marginal cost lower the incentive threshold such that the current monopoly scope/duration exceeds what the scaffold justification requires?',
    'Empirical studies of creator income under varying term lengths and fair use regimes in digital markets; comparison with pre-digital baseline.',
    'If the incentive threshold has fallen but statutory terms have risen, the constraint''s extractiveness under this reading increases — the scaffold becomes overextended.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(digital_incentive_threshold, empirical, 'Whether the scaffold''s calibration remains valid under digital economics.').

omega_variable(
    cs_framing_underdetermination,
    'Is the Copyright Clause''s kernel best framed as the constitutional text itself, or as the institutional practice of copyright law that has grown around it?',
    'Compare the scaffold reading''s fidelity to the constitutional text versus its fidelity to the lived institutional practice. If the practice has drifted from the text, which is the ''real'' kernel?',
    'If the kernel is the text, the scaffold reading is the faithful reading and enclosure is drift. If the kernel is the practice, the scaffold reading is a normative reconstruction. This changes whether the enclosure reading is a mutation or a revelation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Whether the commitment-system kernel is the constitutional text or the institutional practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_constitutional_mandate__public_scaffold_reading, 1790, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ccm_psr_tr_t1790, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 1790, 0.05).
narrative_ontology:measurement(ccm_psr_tr_t1831, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 1831, 0.07).
narrative_ontology:measurement(ccm_psr_tr_t1909, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 1909, 0.11).
narrative_ontology:measurement(ccm_psr_tr_t1976, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 1976, 0.15).
narrative_ontology:measurement(ccm_psr_tr_t1998, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 1998, 0.28).
narrative_ontology:measurement(ccm_psr_tr_t2024, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 2024, 0.12).

% Extraction over time
narrative_ontology:measurement(ccm_psr_be_t1790, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 1790, 0.08).
narrative_ontology:measurement(ccm_psr_be_t1831, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 1831, 0.12).
narrative_ontology:measurement(ccm_psr_be_t1909, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 1909, 0.18).
narrative_ontology:measurement(ccm_psr_be_t1976, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 1976, 0.22).
narrative_ontology:measurement(ccm_psr_be_t1998, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 1998, 0.35).
narrative_ontology:measurement(ccm_psr_be_t2024, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 2024, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(ccm_psr_su_t1790, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 1790, 0.1).
narrative_ontology:measurement(ccm_psr_su_t1831, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 1831, 0.12).
narrative_ontology:measurement(ccm_psr_su_t1909, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 1909, 0.18).
narrative_ontology:measurement(ccm_psr_su_t1976, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 1976, 0.22).
narrative_ontology:measurement(ccm_psr_su_t1998, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 1998, 0.35).
narrative_ontology:measurement(ccm_psr_su_t2024, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 2024, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_constitutional_mandate__public_scaffold_reading, resource_allocation).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__public_scaffold_reading, copyright_constitutional_mandate__corporate_enclosure_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__public_scaffold_reading, copyright_constitutional_mandate__judicial_ambiguity_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__public_scaffold_reading, fair_use_doctrine).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__public_scaffold_reading, public_domain_dedication_tools).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__public_scaffold_reading, orphan_works_legislation).

% DUAL FORMULATION NOTE:
% This constraint is one member of the copyright_constitutional_mandate family. The ε-invariance principle requires separate stories because the corporate enclosure reading authors high extractiveness (property right logic), the judicial ambiguity reading authors moderate extractiveness (legislative deference logic), and this reading authors low-to-moderate extractiveness (scaffold coordination logic). They share the same constitutional text but instantiate different constraints with different beneficiary/victim structures and different ε values. All three link to each other via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
