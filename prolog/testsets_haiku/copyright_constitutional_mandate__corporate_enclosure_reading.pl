% ============================================================================
% CONSTRAINT STORY: copyright_constitutional_mandate__corporate_enclosure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_copyright_constitutional_mandate__corporate_enclosure_reading, []).

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
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: copyright_constitutional_mandate__corporate_enclosure_reading
 *   human_readable: Copyright as Maximal Property Right: Corporate Enclosure Reading
 *   domain: intellectual_property/constitutional_law/political_economy
 *
 * SUMMARY:
 *   The corporate-enclosure reading of the copyright constitutional mandate
 *   treats copyright as a property right demanding maximal protection,
 *   interprets 'limited times' as 'the longest duration Congress can extend
 *   before explicit perpetuity,' and justifies term extension, DMCA
 *   circumvention criminalisation, and fair-use restriction as necessary to
 *   protect property rights. This reading benefits major media corporations
 *   (Disney, RIAA, MPAA) by granting them durable monopolies on derivative
 *   uses and criminally enforceable control over circumvention. The reading
 *   extracts from educators, derivative creators, archivists, and the public
 *   domain by restricting their ability to build on existing culture. The
 *   measurement trajectory shows extractiveness accumulating from ~0.15 (1790
 *   founding) to 0.81 (2026), driven by successive term extensions (1976,
 *   1998, 2019) and the layering of technical enforcement (DMCA 1998) on top
 *   of legal duration. Theater increases from ~0.05 to 0.48, indicating a
 *   growing gap between stated justifications (incentivizing creation) and
 *   actual operation (protecting incumbent rents). Suppression rises from
 *   0.20 to 0.76 as enforcement machinery intensifies: from court cases to
 *   DMCA criminal penalties to anti-circumvention technologies. This is ONE
 *   reading of the contested kernel; sibling readings (public-scaffold,
 *   judicial-ambiguity) interpret the same constitutional text and
 *   institutional history differently and would generate different constraint
 *   structures.
 *
 * KEY AGENTS:
 *   - major_media_corporations: agenda-setter, institutional power, arbitrage exit; direct beneficiary of term extension and DMCA enforcement
 *   - derivative_creators: payer, moderate power, constrained exit; structurally excluded from policy; face legal risk for cultural reuse
 *   - educators: payer, organized power, constrained exit; absorb licensing costs and legal uncertainty
 *   - archivists: payer, moderate power, identity-locked exit; mission conflict—preservation is legally impossible for long-term copyrights
 *   - congress: dual-role agenda-setter/beneficiary via lobbying capture; retains formal authority but lacks political will to revise
 *   - courts: observer, institutional power; defer to Congress via rational-basis review, permitting the reading to persist unchecked
 *   - public domain: non-agent payer; the structural victim—depletion is the constraint's primary effect
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_constitutional_mandate__corporate_enclosure_reading, 0.81).
domain_priors:suppression_score(copyright_constitutional_mandate__corporate_enclosure_reading, 0.76).
domain_priors:theater_ratio(copyright_constitutional_mandate__corporate_enclosure_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_constitutional_mandate__corporate_enclosure_reading, tangled_rope).
narrative_ontology:human_readable(copyright_constitutional_mandate__corporate_enclosure_reading, "Copyright as Maximal Property Right: Corporate Enclosure Reading").
narrative_ontology:topic_domain(copyright_constitutional_mandate__corporate_enclosure_reading, "intellectual_property/constitutional_law/political_economy").

domain_priors:requires_active_enforcement(copyright_constitutional_mandate__corporate_enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__corporate_enclosure_reading, '12d45340-70e5-4022-b45c-d227ac7629c2').
narrative_ontology:cs_kernel_codification('12d45340-70e5-4022-b45c-d227ac7629c2', fixed_text).
narrative_ontology:cs_authority_grounding('12d45340-70e5-4022-b45c-d227ac7629c2', lineage).
narrative_ontology:cs_interpretation_layer_present('12d45340-70e5-4022-b45c-d227ac7629c2').
narrative_ontology:cs_reading_relation('12d45340-70e5-4022-b45c-d227ac7629c2', copyright_constitutional_mandate__public_scaffold_reading, coexists_with).
narrative_ontology:cs_reading_relation('12d45340-70e5-4022-b45c-d227ac7629c2', copyright_constitutional_mandate__judicial_ambiguity_reading, influences).
narrative_ontology:cs_axiom('12d45340-70e5-4022-b45c-d227ac7629c2', foundational, copyright_as_maximal_property).
narrative_ontology:cs_axiom_status(copyright_as_maximal_property, holdable).
narrative_ontology:cs_axiom_grounding('12d45340-70e5-4022-b45c-d227ac7629c2', copyright_as_maximal_property, deontological).
narrative_ontology:cs_axiom('12d45340-70e5-4022-b45c-d227ac7629c2', secondary, incumbency_preserving_interpretation).
narrative_ontology:cs_axiom_status(incumbency_preserving_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('12d45340-70e5-4022-b45c-d227ac7629c2', incumbency_preserving_interpretation, conventional).
narrative_ontology:cs_reference_frame('12d45340-70e5-4022-b45c-d227ac7629c2', property_rights_supreme_authority).
narrative_ontology:cs_drift_state('12d45340-70e5-4022-b45c-d227ac7629c2', contemporary_commons_critique, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('12d45340-70e5-4022-b45c-d227ac7629c2', '2026-06-12T14:32:15Z').
narrative_ontology:cs_kernel_id(copyright_constitutional_mandate__corporate_enclosure_reading, copyright_constitutional_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__corporate_enclosure_reading, major_media_corporations).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__corporate_enclosure_reading, entertainment_incumbents).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, derivative_creators).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, educators).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, archivists).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, researchers).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, remix_culture).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(copyright_constitutional_mandate__corporate_enclosure_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(copyright_constitutional_mandate__corporate_enclosure_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(copyright_constitutional_mandate__corporate_enclosure_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(copyright_constitutional_mandate__corporate_enclosure_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(copyright_constitutional_mandate__corporate_enclosure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.81) because the corporate-enclosure reading operates as a pure transfer: longer terms extend monopoly rents far beyond the point where marginal creation incentives apply; corporate successors collect monopoly gains the original creators never anticipated; and circumvention criminalisation makes the monopoly technically unassailable even when rights holder status is unclear. Suppression is high (0.76) because the constraint's persistence depends critically on active enforcement: DMCA prosecution deters circumvention; lobbying maintains Congressional extension; litigation chills derivative use. The constraint would not persist without active legal and technical defense. Theater is moderate-rising (0.48 at endpoint, up from 0.05 at founding) because the stated rationale—incentivizing creation—decouples sharply from operation. Successive term extensions are defended rhetorically as creation incentives while economic evidence shows the incentive is saturated and further extension extracts pure rent. Theater rises as the gap widens. Accessibility collapse is moderate (0.72) because alternatives do exist legally (fair use, public domain works, licensing) but are costly, risky, or shrinking. Derivative creators have exit routes (use older works, license expensive, go international) but each is constrained. Resistance is moderately high (0.68) because the reading faces consistent contestation: Eldred v. Ashcroft challenged term extension on constitutional grounds; remix and open-culture movements contest the maximal-protection framing; scholars publish critical economic analysis; but resistance has been unable to change the fundamental structure. The temporal measurements trace extraction accumulation: minimal in 1790 (14-year term, limited scope), rising in 1870 (first international harmonization), accelerating in 1976 (life+50), spiking in 1998 (DMCA, Sonny Bono Act, 20-year extension), and consolidating by 2026. Theater rises fastest after 1998, when DMCA criminalization severed the link between copyright duration and circumvention incentives—the enforcement now operates purely as suppression, not coordination.
 *
 * PERSPECTIVAL GAP:
 *   The major media corporations and Congress perceive the constraint as legitimate property protection grounded in constitutional authorization. From their perspective, strong copyright is coordination infrastructure: creators get monopoly incentive, users get ordered distribution, corporations get business stability. From the derivative creators' and educators' seats, the same structure operates as enforced extraction: they are forbidden from reuse not because they lack legal standing but because the corporation maintains monopoly by force (courts, DMCA, technical barriers). The agenda-setter (corporate incumbents) has exit via arbitrage (they can relocate, license internationally, diversify) while the payers (derivative creators, archivists) face trapped or identity-locked exit. The engine computes these as sharply different directionalities: the beneficiary seats see low d (subsidy), the payer seats see high d (extraction). Courts occupy an observer seat that sees both frames but have chosen to defer to Congress, treating the issue as a rational-basis question rather than a structural conflict. This deference permits the corporate-enclosure reading to dominate without judicial constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Major media corporations derive directionality d ≈ 0.0–0.15 (full beneficiary): they set the rules, collect the monopoly rent, have arbitrage exit (can relocate, can license globally). Derivative creators derive d ≈ 0.85–0.95 (near-full target): they pay legal risk and creative constraint, cannot exit without abandoning the medium, have no voice in rule-setting. Educators and archivists derive d ≈ 0.75–0.85 (high targets): they absorb licensing costs and legal uncertainty, have constrained or identity-locked exit, and bear the accumulating burden of long copyright terms. The public domain (non-agent but included for narrative completeness) has d ≈ 1.0 (complete target): it is the structural victim, its depletion is the constraint's primary effect. Congress occupies a dual position: as rule-setter (d ≈ 0.2, beneficiary via lobbying dynamics) and as representative institution (d ≈ 0.5, symmetric if Congress truly represented constituent interests). The divergence in directionalities is the key structural signature: the constraint benefits those who set it and harms those who have no seat at the table.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—incentivizing creation without permanent monopoly—is demonstrably dead. Three lines of evidence: (1) Empirical: copyright terms have extended far beyond the point where marginal creation incentives apply; 1998's Sonny Bono Act added 20 years to existing works (retroactive extension), which obviously cannot have altered the creation incentive for works already created. (2) Economic: studies of creator behavior and copyright elasticity (Nordhaus, Landes & Posner, Boldrin & Levine) show the incentive curve saturates around 20 years; further extension produces negligible marginal incentive. (3) Institutional: Congress's term-extension votes have tracked media industry lobbying rather than evidence; Congressional Record shows no new evidence presented in the 1998 extension debates that would justify adding 20 years. The mandate has died, but the constraint persists. This is the mandatrophy signature: the institution designed to solve a coordination problem continues to operate after the problem is solved, now functioning as pure extraction. The constraint prevents mislabeling this as 'rope' (genuine coordination) by requiring that we identify the coordinate problem being solved; when the founding problem is dead, the coordination framing collapses. The corporate-enclosure reading explicitly embraces this: it frames copyright as property right, not as incentive mechanism, thus dispensing with the need for a live coordination problem. This honesty (the reading admits it is extraction) is the cost of the reading's internal coherence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    property_vs_incentive_frame,
    'Is copyright fundamentally a property right (analogous to land or chattels) or an incentive mechanism (analogous to a patent grant or tax credit)? The reading''s core premise treats it as property. What empirical or structural fact would establish which framing is correct?',
    'Examine the constitutional history (did the Framers intend property or incentive language?), the economic structure (do copyright''s effects match property law or incentive-grant law?), and the institutional trajectory (does behavior show incumbent protection or incentive calibration?). Different sibling readings privilege different historical sources and different economic measures.',
    'If copyright is fundamentally property, maximal protection is justified and fair-use restriction is legitimate. If fundamentally incentive, maximal protection becomes irrational once the incentive is saturated. This omega is the structural hinge between readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_vs_incentive_frame, conceptual, 'Whether copyright is grounded in property rights or incentive mechanisms—the foundational distinction between the corporate-enclosure and public-scaffold readings.').

omega_variable(
    marginal_incentive_saturation,
    'At what copyright term length does the marginal creation incentive saturate—that is, when does additional term length produce negligible additional incentive to create? Current law provides 95 years for corporate works. Is the incentive marginal at 95 years, or does the incentive remain substantial?',
    'Economic studies of creator behavior, survey data on creator motivation, and comparative analysis of creation rates across jurisdictions with different copyright terms. The challenge: most creators are risk-averse and plan for decades; isolating the marginal effect of the 70th year vs. the 100th year requires careful study.',
    'If saturation occurs before 95 years (evidence suggests ~20 years), the corporate-enclosure reading loses its empirical justification: extending terms is pure extraction, not coordination. If saturation occurs at or after 95 years, the reading''s extraction claim is weaker. This omega is primarily empirical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(marginal_incentive_saturation, empirical, 'The point at which copyright term extension ceases to produce marginal creation incentive, establishing whether further extension is extraction.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of derivative use structural (external barriers: DMCA penalties, litigation risk, licensing costs) or internalized (creators have absorbed ''copyright is sacred'' norms and self-censor even when fair use would be legal)? The measured suppression is 0.76; what fraction is each?',
    'Post-circumvention experiment: if DMCA were repealed while term extension remained, would derivative reuse spike? If creators continue self-censoring, suppression is internalized. If reuse explodes, suppression was structural. Survey data on creator understanding of fair use (many believe fair use is narrower than it is) indicates significant internalization.',
    'If suppression is structural, removing enforcement infrastructure (DMCA, litigation campaigns) would liberate reuse immediately. If internalized, cultural narratives about copyright would need to shift as well—a longer road. This omega determines what fixing the constraint would actually require.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether measured suppression is structural (external barriers) or internalized (cultural norms), affecting the feasibility of remedies.').

omega_variable(
    reading_kernel_distinction,
    'Is the corporate-enclosure reading of the copyright constitutional mandate a distinct constraint (a different reading of the same kernel), or is it the same constraint as the public-scaffold reading, just with different metrics? The distinction matters for constraint family decomposition (OQ-84).',
    'Examine whether the two readings instantiate the same beneficiary/victim structure, the same coordination function, and the same extraction mechanism. If they differ structurally (they do: beneficiary is incumbent vs. public; extraction is rent vs. information scarcity), they are different constraints. If they differ only in evaluation (same structure, different judgments), they are the same constraint viewed differently.',
    'If distinct constraints, the corpus should include both readings as separate stories linked via network.affects_constraints (Rule 1: constraint families). If the same constraint, only one JSON file should exist with the reading declared as an omega. The ε-invariance principle applies: the two readings have detectably different epsilon values (corporate-enclosure is ~0.81, public-scaffold is ~0.35), confirming they are different constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_distinction, conceptual, 'Whether the corporate-enclosure reading and public-scaffold reading are structurally distinct constraints or alternative evaluations of the same constraint.').

omega_variable(
    congress_capture_vs_rational_policy,
    'Is Congress''s repeated copyright extension driven by rational policy judgment (Congress genuinely believes longer terms incentivize creation) or by incumbent lobbying capture (Congress is responding to campaign contributions and revolving-door dynamics)? Both could be true partially, but where does the primary causal weight lie?',
    'Examine Congressional voting patterns (has voting correlated with districts'' media industry presence?), lobbying expenditures (media industry vs. derivative creators), and campaign contributions (Disney, RIAA donations to copyright-extension supporters). Compare stated justifications in floor debate with contemporary evidence (did evidence change between 1976 and 1998? No—the justifications are constant while evidence evolved).',
    'If captured, the constraint''s persistence reflects power imbalance, not policy merit, and the appropriate remedy is structural (campaign finance reform, direct representation for public-domain interests). If rational policy, the constraint reflects legitimate Congressional discretion and remedies must focus on changing Congress''s mind via evidence. The truth likely contains both, but distinguishing the proportion is crucial for remedy design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(congress_capture_vs_rational_policy, empirical, 'Whether Congressional copyright extension reflects policy judgment or incumbent capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_constitutional_mandate__corporate_enclosure_reading, 1790, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(copy_tr_t1790, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 1790, 0.05).
narrative_ontology:measurement(copy_tr_t1870, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 1870, 0.08).
narrative_ontology:measurement(copy_tr_t1976, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 1976, 0.18).
narrative_ontology:measurement(copy_tr_t1998, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 1998, 0.35).
narrative_ontology:measurement(copy_tr_t2013, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 2013, 0.42).
narrative_ontology:measurement(copy_tr_t2026, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 2026, 0.48).

% Extraction over time
narrative_ontology:measurement(copy_be_t1790, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 1790, 0.15).
narrative_ontology:measurement(copy_be_t1870, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 1870, 0.22).
narrative_ontology:measurement(copy_be_t1976, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 1976, 0.45).
narrative_ontology:measurement(copy_be_t1998, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 1998, 0.68).
narrative_ontology:measurement(copy_be_t2013, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 2013, 0.76).
narrative_ontology:measurement(copy_be_t2026, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 2026, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(copy_su_t1790, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 1790, 0.2).
narrative_ontology:measurement(copy_su_t1870, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 1870, 0.28).
narrative_ontology:measurement(copy_su_t1976, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 1976, 0.42).
narrative_ontology:measurement(copy_su_t1998, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 1998, 0.61).
narrative_ontology:measurement(copy_su_t2013, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 2013, 0.71).
narrative_ontology:measurement(copy_su_t2026, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 2026, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_constitutional_mandate__corporate_enclosure_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(copyright_constitutional_mandate__corporate_enclosure_reading, 0.12).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, copyright_constitutional_mandate__public_scaffold_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, copyright_constitutional_mandate__judicial_ambiguity_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, dmca_anti_circumvention).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, fair_use_doctrine).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, creative_commons_alternative_licensing).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'copyright_constitutional_mandate'. The corporate-enclosure reading interprets copyright as a property right requiring maximal protection and 'limited times' as the longest politically feasible term. The sibling public-scaffold reading interprets copyright as a public-goods mechanism and 'limited times' as a short temporary monopoly. The sibling judicial-ambiguity reading treats 'limited times' as within Congressional rational discretion. All three instantiate the same constitutional text but produce different constraint structures (beneficiaries, victims, epsilon values). Each story must be authored separately per the ε-invariance principle (OQ-84); they are linked via network.affects_constraints and documented in the committer frame (cs_structure.reading_relations). This story is the corporate-enclosure reading; epsilon ≈ 0.81. The public-scaffold reading would have epsilon ≈ 0.35 (genuine coordination, minimal extraction). The structural difference is not observable-dependent but interpretation-dependent: which reading of the constitutional mandate is operative determines which constraint structure instantiates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(copyright_constitutional_mandate__corporate_enclosure_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
