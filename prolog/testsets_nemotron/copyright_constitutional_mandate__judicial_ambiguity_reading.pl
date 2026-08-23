% ============================================================================
% CONSTRAINT STORY: copyright_constitutional_mandate__judicial_ambiguity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_copyright_constitutional_mandate__judicial_ambiguity_reading, []).

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
 *   constraint_id: copyright_constitutional_mandate__judicial_ambiguity_reading
 *   human_readable: Copyright Term as Zone of Legislative Discretion under Rational Basis Review
 *   domain: intellectual_property/constitutional_law/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the judicial_ambiguity_reading of the
 *   copyright_constitutional_mandate kernel. The reading holds that Article
 *   I, Section 8, Clause 8's 'limited times' language creates a zone of
 *   legislative discretion within which Congress may set copyright terms, and
 *   that courts owe rational basis deference to congressional judgments about
 *   what duration promotes the progress of science and useful arts. This
 *   reading was authoritatively articulated in Eldred v. Ashcroft (2003),
 *   where the Supreme Court upheld the Sonny Bono Copyright Term Extension
 *   Act's 20-year addition to existing terms. The constraint coordinates
 *   legitimate legislative flexibility in copyright policy while
 *   simultaneously extracting value from the public domain by enabling serial
 *   term extensions that approach perpetuity in practice. Beneficiaries are
 *   congressional authority as an institutional actor and the copyright
 *   holder lobbies that successfully petition for extensions. Victims are the
 *   constitutional fixity that 'limited times' was meant to provide, public
 *   domain users who lose access to works, independent creators who cannot
 *   build on cultural commons, and archival institutions that face perpetual
 *   clearance burdens.
 *
 * KEY AGENTS:
 *   - congressional_authority: Primary agenda setter (institutional/analytical) — sets term lengths via legislation
 *   - copyright_holder_lobbies: Primary beneficiary (organized/generational) — petitions for and captures term extensions
 *   - legacy_content_industries: Secondary beneficiary (institutional/generational) — derive recurring revenue from extended monopolies
 *   - constitutional_fixity_as_drift_constraint: Primary victim (analytical/civilizational) — the textual limit that should constrain drift but does not
 *   - public_domain_users: Victim (organized/biographical) — lose access to works that would have entered public domain
 *   - independent_creators: Victim (moderate/biographical) — cannot build on cultural commons locked by extensions
 *   - archival_institutions: Victim (organized/biographical) — face perpetual clearance costs for preservation
 *   - federal_courts: Observer (analytical/analytical) — apply rational basis deference, do not set terms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.38).
domain_priors:suppression_score(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.42).
domain_priors:theater_ratio(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_constitutional_mandate__judicial_ambiguity_reading, tangled_rope).
narrative_ontology:human_readable(copyright_constitutional_mandate__judicial_ambiguity_reading, "Copyright Term as Zone of Legislative Discretion under Rational Basis Review").
narrative_ontology:topic_domain(copyright_constitutional_mandate__judicial_ambiguity_reading, "intellectual_property/constitutional_law/political_economy").

domain_priors:requires_active_enforcement(copyright_constitutional_mandate__judicial_ambiguity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__judicial_ambiguity_reading, '2130702c-92a0-48cf-b701-27038439d854').
narrative_ontology:cs_kernel_codification('2130702c-92a0-48cf-b701-27038439d854', fixed_text).
narrative_ontology:cs_authority_grounding('2130702c-92a0-48cf-b701-27038439d854', lineage).
narrative_ontology:cs_interpretation_layer_present('2130702c-92a0-48cf-b701-27038439d854').
narrative_ontology:cs_reading_relation('2130702c-92a0-48cf-b701-27038439d854', copyright_constitutional_mandate__corporate_enclosure_reading, influences).
narrative_ontology:cs_reading_relation('2130702c-92a0-48cf-b701-27038439d854', copyright_constitutional_mandate__public_scaffold_reading, coexists_with).
narrative_ontology:cs_axiom('2130702c-92a0-48cf-b701-27038439d854', foundational, rational_basis_deference_satisfies_limited_times).
narrative_ontology:cs_axiom_status(rational_basis_deference_satisfies_limited_times, holdable).
narrative_ontology:cs_axiom_grounding('2130702c-92a0-48cf-b701-27038439d854', rational_basis_deference_satisfies_limited_times, conventional).
narrative_ontology:cs_axiom('2130702c-92a0-48cf-b701-27038439d854', foundational, congressional_discretion_includes_serial_extension).
narrative_ontology:cs_axiom_status(congressional_discretion_includes_serial_extension, holdable).
narrative_ontology:cs_axiom_grounding('2130702c-92a0-48cf-b701-27038439d854', congressional_discretion_includes_serial_extension, conventional).
narrative_ontology:cs_reference_frame('2130702c-92a0-48cf-b701-27038439d854', founding_era_copyright_bargain).
narrative_ontology:cs_drift_state('2130702c-92a0-48cf-b701-27038439d854', post_eldred_2003, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2130702c-92a0-48cf-b701-27038439d854', '').
narrative_ontology:cs_kernel_id(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_constitutional_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__judicial_ambiguity_reading, congressional_authority).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_holder_lobbies).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__judicial_ambiguity_reading, legacy_content_industries).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__judicial_ambiguity_reading, constitutional_fixity_as_drift_constraint).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__judicial_ambiguity_reading, public_domain_users).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__judicial_ambiguity_reading, independent_creators).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__judicial_ambiguity_reading, archival_institutions).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__judicial_ambiguity_reading, congressional_supremacy_in_copyright_policy).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__judicial_ambiguity_reading, rational_basis_deference_doctrine).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__judicial_ambiguity_reading, eldred_precedent_stare_decisis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets copyright term lengths through legislation. Holds the constitutional power to define 'limited times.' Benefits from maximal legislative discretion — the deference doctrine insulates its judgments from judicial second-guessing. Can change the constraint at any time by enacting different terms; exit from the constraint is trivial (legislate differently) but politically costly due to lobby pressure.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, congressional_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Organized industry groups (MPAA, RIAA, Authors Guild, etc.) that petition Congress for term extensions. Capture the economic gains of extended monopolies. Their exit from the constraint is mobile — they could advocate for different policies — but their business models are structured around long-term exclusive rights, creating identity_locked dynamics at the organizational level.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_holder_lobbies, beneficiary,
    organized, generational, mobile, national).

% Major studios, publishers, and record labels with large catalogs of works approaching public domain. Derive recurring revenue from term extensions. Their exit is constrained: they could adapt to public domain enrichment models, but their valuation models and shareholder expectations are built on perpetual copyright-like revenue streams.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, legacy_content_industries, beneficiary,
    institutional, generational, constrained, global).

% The structural function of the 'limited times' text as a genuine constraint on legislative drift. Pays the cost of the constraint's extraction: its constraining force is nullified by the deference doctrine while its textual presence legitimates the system. Has no exit — it is a structural position, not an agent. Its 'situation' is the gap between textual promise and operational reality.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, constitutional_fixity_as_drift_constraint, payer,
    analytical, civilizational, analytical, universal).

% Educators, researchers, artists, librarians, and general public who would access works entering the public domain. Bear diffuse, cumulative costs: each extension delays access by 20 years. Exit is constrained: they can use fair use, public domain works, or creative commons, but the cultural commons shrinks relative to what the constitutional bargain promised. No effective collective action mechanism to counter organized lobby pressure.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, public_domain_users, payer,
    organized, biographical, constrained, global).

% Creators who build on existing culture but lack the licensing resources of major studios. Bear the cost of a cultural commons that recedes rather than expands. Exit is constrained: they can use public domain works, but the pool shrinks with each extension; they cannot opt out of the copyright system that governs their own works.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, independent_creators, payer,
    moderate, biographical, constrained, global).

% Libraries, archives, museums tasked with preserving cultural heritage. Face perpetual clearance burdens for works that should be in public domain. Each extension adds 20 years of copyright management costs. Exit is constrained: they have statutory exceptions (Section 108) but these are narrow and do not cover general public access.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, archival_institutions, payer,
    organized, biographical, constrained, national).

% Apply rational basis review to copyright term legislation. Do not set terms or capture extraction. Their structural position is to legitimize the constraint through deference. Could change the constraint by adopting heightened scrutiny, but institutional norms and precedent (Eldred) constrain this exit.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, federal_courts, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_holder_lobbies).
narrative_ontology:fixing_cost_class(copyright_constitutional_mandate__judicial_ambiguity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a national copyright system by vesting term-setting authority in Congress, avoiding judicial micromanagement of cultural policy, and providing a stable framework for creators and investors to plan around.
% TRANSFER_FUNCTION: Moves the value of works that would have entered the public domain (access, reuse, adaptation, preservation) from the public and downstream creators to copyright holders and their commercial licensees, via the mechanism of legislative term extension ratified by judicial deference.
% ABSENT_VOICES: Future generations who would inherit a richer public domain; creators in developing countries who cannot access cultural works locked by U.S. term extensions exported via trade agreements; the constitutional text itself ('limited times') which has no voice in its own interpretation. These voices are structurally excluded — future generations cannot lobby, foreign creators lack standing, and the text is interpreted by the very institutions the constraint empowers.
% DISAPPEARANCE_RATIONALE: If rational basis deference for copyright terms vanished overnight, courts would apply heightened scrutiny to term extensions. The Sonny Bono Act and similar extensions would likely be invalidated. Works would enter the public domain on the original schedule. The cultural commons would expand. Congress would lose its insulation from judicial review on copyright policy. The copyright industries would lose a reliable mechanism for serial term extension. The entire political economy of copyright term setting would reorganize around a meaningful 'limited times' constraint.
% FOUNDING_PROBLEM: The Constitution's Copyright Clause was designed to solve the problem of incentivizing creation while ensuring the public domain expands: 'limited times' was the mechanism balancing author monopoly with public access. The founding problem was how to grant enough monopoly to incentivize creation without locking up culture perpetually.
% FOUNDING_PROBLEM_CORROBORATION: The public scaffold reading and its adherents (public domain advocates, library associations, digital rights organizations, legal scholars like Lessig, Boyle, Litman) attest the founding problem remains live — the balance is broken. The corporate enclosure reading and its adherents (major content industries, some IP maximalist scholars) attest the founding problem was never about balance but about securing property rights. The judicial ambiguity reading (this reading's proponents: the Eldred majority, congressional leadership, Copyright Office) attest the problem is solved by any non-perpetual term. No single corroborating source outside the beneficiary set affirms that serial 20-year extensions satisfy 'limited times'; the Eldred dissent (Breyer, Stevens) and subsequent scholarship provide external corroboration that the founding problem is not solved.
narrative_ontology:disappearance_verdict(copyright_constitutional_mandate__judicial_ambiguity_reading, world_rearranges).
narrative_ontology:founding_problem_status(copyright_constitutional_mandate__judicial_ambiguity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(copyright_constitutional_mandate__judicial_ambiguity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(copyright_constitutional_mandate__judicial_ambiguity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(copyright_constitutional_mandate__judicial_ambiguity_reading_tests).
:- end_tests(copyright_constitutional_mandate__judicial_ambiguity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38) is low-to-moderate because the constraint's surface function — coordinating legislative copyright policy — is genuine, and the extraction from the public domain is diffuse and delayed. Suppression (0.42) is moderate because the constraint's persistence depends on active judicial enforcement of deference (courts must actively refuse to invalidate extensions) and legislative action (Congress must be lobbied to extend), but alternatives (shorter terms, formalities) are not actively crushed — they are simply not enacted. Theater ratio (0.28) reflects that the 'limited times' language performs a constraining function it no longer delivers; the judicial opinion in Eldred treats the clause as meaningful while the operational outcome approaches perpetuity. Accessibility collapse (0.35) is low because alternative copyright frameworks (formalities, shorter terms, compulsory licensing) remain conceptually and legally available — they are politically blocked, not structurally foreclosed. Resistance (0.55) is moderate: public domain advocates, librarians, and some scholars actively contest the reading, but the constraint holds because the institutional beneficiaries (Congress, courts, content industries) are aligned.
 *
 * PERSPECTIVAL GAP:
 *   From congressional_authority's seat (institutional, arbitrage exit), the constraint is a rope: genuine coordination of copyright policy with minimal coercive overhead. From constitutional_fixity_as_drift_constraint's seat (analytical, analytical exit), it is a snare: the limit exists textually but is operationally null, and the deference doctrine is the mechanism of nullification. From copyright_holder_lobbies' seat (organized, generational), it is a rope that delivers reliable extraction. The engine computes this divergence from the structural data — the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Congressional authority is the structural beneficiary (d near 0.0): the constraint subsidizes its legislative discretion. Copyright holder lobbies and legacy industries are beneficiaries (d ~ 0.15): they capture the gains of extensions through the legislative process they influence. Constitutional fixity is the primary victim (d ~ 0.9): the constraint extracts the constraining force the text was meant to provide. Public domain users, independent creators, and archival institutions are victims (d ~ 0.7-0.8): they bear diffuse, cumulative costs with constrained exit (cannot opt out of copyright system). Federal courts sit near symmetric (d ~ 0.5): they perform the deference function but do not capture the extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The 'limited times' clause shows strong mandatrophy signals: the textual mandate persists while its constraining function has atrophied. The constraint was built to solve the founding problem of balancing author incentive with public access through a genuine temporal limit. That problem is contested — the public scaffold reading says it remains live; the corporate enclosure reading says it was never the point; this reading says the limit is satisfied by any non-perpetual term. No external corroboration supports the claim that 'limited times' is satisfied by life+70 plus serial extensions; the clause's operational meaning has drifted from its founding function without constitutional acknowledgment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the copyright constitutional mandate kernel, or does it collapse into the corporate enclosure or public scaffold readings?',
    'Compare structural elements: beneficiary/victim sets, epsilon values, and coordination-extraction boundaries across the three declared readings. A reading is distinct if it has a stable epsilon, its own beneficiary/victim structure, and its own classification without averaging over siblings.',
    'If the readings are not structurally distinct, the kernel decomposition fails and the constraint family should be collapsed; if distinct, each reading instantiates its own constraint with independent metrics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Structural distinctness of the judicial_ambiguity_reading within the copyright_constitutional_mandate kernel family').

omega_variable(
    judicial_deference_as_enabling_mechanism,
    'Does rational basis deference functionally enable the scaffold-to-enclosure transition, or is it merely a procedural posture with no material effect on term extension outcomes?',
    'Empirical analysis of term extension enactments post-Eldred: did any extension fail rational basis review? Counterfactual: would a heightened scrutiny standard have produced different legislative outcomes?',
    'If deference materially enables extension, the reading''s extraction is higher than its surface coordination function suggests; if merely procedural, the constraint is closer to a rope with theatrical enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_deference_as_enabling_mechanism, empirical, 'Whether judicial deference doctrine has causal force in the copyright term extension trajectory').

omega_variable(
    sibling_reading_boundary,
    'Where exactly does this reading''s beneficiary set diverge from the corporate_enclosure_reading''s beneficiary set, given both benefit congressional authority?',
    'Trace the coordination function: this reading coordinates legislative flexibility for any copyright policy; corporate_enclosure_reading coordinates maximal protection for rights holders specifically. The structural delta is whether the beneficiary is ''congressional authority as such'' or ''copyright holders via congressional authority''.',
    'If the boundary is porous, the readings may be collapsed in practice despite theoretical distinction; if sharp, the readings have different extraction profiles and different victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_boundary, conceptual, 'Beneficiary structure boundary between judicial_ambiguity_reading and corporate_enclosure_reading').

omega_variable(
    mandatrophy_of_limited_times,
    'Has the constitutional ''limited times'' clause suffered mandatrophy — does it persist as a textual limit while its constraining function has atrophied?',
    'Track the gap between textual limit (''limited times'') and operational limit (life+70, with no enacted extension ever invalidated). If the gap widens without constitutional invalidation, the clause functions as theater.',
    'If mandatrophy is confirmed, the constraint''s theater_ratio is understated and the classification shifts toward piton; if the limit remains operative, the reading''s coordination function remains genuine.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandatrophy_of_limited_times, empirical, 'Whether ''limited times'' retains constraining force or has become performative text').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_constitutional_mandate__judicial_ambiguity_reading, 1790, 2003).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(copyright_constitutional_mandate__judicial_ambiguity_reading_tr_t1790, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 1790, 0.05).
narrative_ontology:measurement(copyright_constitutional_mandate__judicial_ambiguity_reading_tr_t1831, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 1831, 0.08).
narrative_ontology:measurement(copyright_constitutional_mandate__judicial_ambiguity_reading_tr_t1909, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 1909, 0.12).
narrative_ontology:measurement(copyright_constitutional_mandate__judicial_ambiguity_reading_tr_t1976, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 1976, 0.18).
narrative_ontology:measurement(copyright_constitutional_mandate__judicial_ambiguity_reading_tr_t1998, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 1998, 0.25).
narrative_ontology:measurement(copyright_constitutional_mandate__judicial_ambiguity_reading_tr_t2003, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 2003, 0.28).

% Extraction over time
narrative_ontology:measurement(copyright_constitutional_mandate__judicial_ambiguity_reading_be_t1790, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 1790, 0.12).
narrative_ontology:measurement(copyright_constitutional_mandate__judicial_ambiguity_reading_be_t1831, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 1831, 0.15).
narrative_ontology:measurement(copyright_constitutional_mandate__judicial_ambiguity_reading_be_t1909, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 1909, 0.22).
narrative_ontology:measurement(copyright_constitutional_mandate__judicial_ambiguity_reading_be_t1976, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 1976, 0.28).
narrative_ontology:measurement(copyright_constitutional_mandate__judicial_ambiguity_reading_be_t1998, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 1998, 0.35).
narrative_ontology:measurement(copyright_constitutional_mandate__judicial_ambiguity_reading_be_t2003, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 2003, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(copyright_constitutional_mandate__judicial_ambiguity_reading_su_t1790, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 1790, 0.15).
narrative_ontology:measurement(copyright_constitutional_mandate__judicial_ambiguity_reading_su_t1831, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 1831, 0.18).
narrative_ontology:measurement(copyright_constitutional_mandate__judicial_ambiguity_reading_su_t1909, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 1909, 0.25).
narrative_ontology:measurement(copyright_constitutional_mandate__judicial_ambiguity_reading_su_t1976, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 1976, 0.32).
narrative_ontology:measurement(copyright_constitutional_mandate__judicial_ambiguity_reading_su_t1998, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 1998, 0.38).
narrative_ontology:measurement(copyright_constitutional_mandate__judicial_ambiguity_reading_su_t2003, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 2003, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_constitutional_mandate__judicial_ambiguity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.12).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_constitutional_mandate__corporate_enclosure_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_constitutional_mandate__public_scaffold_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_term_extension_act_1998).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__judicial_ambiguity_reading, eldred_v_ashcroft_precedent).

% DUAL FORMULATION NOTE:
% This reading is one of three in the copyright_constitutional_mandate constraint family. The kernel 'limited times' decomposes into: (1) public_scaffold_reading — low epsilon, mountain-like coordination; (2) judicial_ambiguity_reading — moderate epsilon, tangled rope (this story); (3) corporate_enclosure_reading — high epsilon, snare. Each reading has distinct beneficiary/victim structures and epsilon values. They are linked via affects_constraints because the judicial_ambiguity reading's deference doctrine structurally enables the corporate_enclosure reading's extensions, while the public_scaffold reading provides the normative counter-pressure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(copyright_constitutional_mandate__judicial_ambiguity_reading, institutional, 0.1).
constraint_indexing:directionality_override(copyright_constitutional_mandate__judicial_ambiguity_reading, organized, 0.15).
constraint_indexing:directionality_override(copyright_constitutional_mandate__judicial_ambiguity_reading, analytical, 0.85).
constraint_indexing:directionality_override(copyright_constitutional_mandate__judicial_ambiguity_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
