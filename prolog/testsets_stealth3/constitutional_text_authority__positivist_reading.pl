% ============================================================================
% CONSTRAINT STORY: constitutional_text_authority__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text_authority__positivist_reading, []).

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
 *   constraint_id: constitutional_text_authority__positivist_reading
 *   human_readable: Positivist Rule of Constitutional Validity (Pedigree over Moral Content)
 *   domain: legal/constitutional/jurisprudential
 *
 * SUMMARY:
 *   A rule of recognition for constitutional law: a provision is valid
 *   constitutional law because it was enacted through the prescribed
 *   procedures by the prescribed institutions, and for no further reason.
 *   Moral merit does not bear on the validity question; a duly enacted unjust
 *   provision is law, and the designed remedy for injustice is amendment, not
 *   moral argument in court. The arrangement is administered by apex courts
 *   that decide which challenges receive a hearing, staffed and rationalized
 *   by a profession whose technical expertise is the reading of enactment
 *   records, relied on by planning interests that price long-horizon
 *   commitments against determinate validity, and borne most heavily by those
 *   who live under valid provisions that treat them unjustly and cannot reach
 *   the amendment procedures that could relieve them. The claim/metric split
 *   is deliberate: claimed_type is authored from the structure (a genuine
 *   coordination function with asymmetric costs that must be actively
 *   maintained), while the metrics are authored as descriptive truths from
 *   this reading's own seat — including a theater ratio that has been
 *   climbing as the strong law/morality separation retreats toward
 *   professional ritual while the procedural core keeps working.
 *
 * KEY AGENTS:
 *   - apex_constitutional_courts: agenda-setter (institutional/constrained) — administers the pedigree test and polices the validity boundary; depends on the boundary it polices
 *   - legal_profession: primary beneficiary (organized/identity_locked) — professional authority rests on law as technical enactment-reading
 *   - incumbent_constitutional_officeholders: beneficiary (powerful/arbitrage) — insulation of duly enacted arrangements from moral delegitimization
 *   - commercial_reliance_interests: beneficiary (organized/constrained) — determinate validity as the substrate of long-horizon planning
 *   - subjects_of_valid_unjust_provisions: primary target (powerless/trapped) — bear valid-but-unjust provisions with amendment as the only remedy
 *   - natural_law_jurists: excluded (moderate/mobile) — their claims are ruled out of order, not answered
 *   - analytical_jurisprudence_scholars: analytical observer — maps the arrangement's social sources and failure modes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text_authority__positivist_reading, 0.38).
domain_priors:suppression_score(constitutional_text_authority__positivist_reading, 0.51).
domain_priors:theater_ratio(constitutional_text_authority__positivist_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, suppression_requirement, 0.51).
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text_authority__positivist_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text_authority__positivist_reading, "Positivist Rule of Constitutional Validity (Pedigree over Moral Content)").
narrative_ontology:topic_domain(constitutional_text_authority__positivist_reading, "legal/constitutional/jurisprudential").

domain_priors:requires_active_enforcement(constitutional_text_authority__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text_authority__positivist_reading, 'b25a4e74-5260-40df-ae82-a921343a49d4').
narrative_ontology:cs_kernel_codification('b25a4e74-5260-40df-ae82-a921343a49d4', fixed_text).
narrative_ontology:cs_authority_grounding('b25a4e74-5260-40df-ae82-a921343a49d4', practice).
narrative_ontology:cs_interpretation_layer_present('b25a4e74-5260-40df-ae82-a921343a49d4').
narrative_ontology:cs_reading_relation('b25a4e74-5260-40df-ae82-a921343a49d4', constitutional_text_authority__originalist_reading, influences).
narrative_ontology:cs_reading_relation('b25a4e74-5260-40df-ae82-a921343a49d4', constitutional_text_authority__living_constitutionalist_reading, forecloses).
narrative_ontology:cs_axiom('b25a4e74-5260-40df-ae82-a921343a49d4', foundational, validity_requires_pedigree_not_merit).
narrative_ontology:cs_axiom_status(validity_requires_pedigree_not_merit, holdable).
narrative_ontology:cs_axiom_grounding('b25a4e74-5260-40df-ae82-a921343a49d4', validity_requires_pedigree_not_merit, conventional).
narrative_ontology:cs_axiom('b25a4e74-5260-40df-ae82-a921343a49d4', secondary, separation_preserves_law_criticism).
narrative_ontology:cs_axiom_status(separation_preserves_law_criticism, holdable).
narrative_ontology:cs_axiom_grounding('b25a4e74-5260-40df-ae82-a921343a49d4', separation_preserves_law_criticism, instrumental).
narrative_ontology:cs_reference_frame('b25a4e74-5260-40df-ae82-a921343a49d4', procedural_pedigree_validity).
narrative_ontology:cs_drift_state('b25a4e74-5260-40df-ae82-a921343a49d4', contemporary_rights_constitutionalism, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('b25a4e74-5260-40df-ae82-a921343a49d4', '').
narrative_ontology:cs_kernel_id(constitutional_text_authority__positivist_reading, constitutional_text_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text_authority__positivist_reading, legal_profession).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__positivist_reading, incumbent_constitutional_officeholders).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__positivist_reading, commercial_reliance_interests).
narrative_ontology:constraint_victim(constitutional_text_authority__positivist_reading, subjects_of_valid_unjust_provisions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__positivist_reading, apex_constitutional_courts).
narrative_ontology:constraint_vindicates(constitutional_text_authority__positivist_reading, legal_positivism_separation_thesis).
narrative_ontology:constraint_vindicates(constitutional_text_authority__positivist_reading, rule_of_recognition_social_fact_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decide which challenges to constitutional provisions receive a hearing and which are answered with the same institutional reply: the provision was duly enacted, so it stands. They police the boundary that keeps moral-merit arguments out of validity questions, and their own authority — final, determinate, reviewable only through the procedures they administer — depends on keeping that boundary. They cannot step outside the legal order they administer; repudiating the boundary would mean repudiating the foundations of their own office, as the post-war reckonings showed at great institutional cost.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, apex_constitutional_courts, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text_authority__positivist_reading, apex_constitutional_courts, beneficiary).

% Train, credential, and employ the people who operate the validity machinery. Their expertise is reading enactment records, procedural history, and institutional sources — a body of technical knowledge whose monopoly would break if moral philosophy became a validity criterion. Generations of professional formation have fused the discipline's identity with the law/morality distinction; a lawyer arguing that validity turns on moral merit argues from outside the discipline they were formed in.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, legal_profession, beneficiary,
    organized, biographical, identity_locked, national).

% Hold office under the enacted constitution and benefit from the rule that their arrangements, once duly enacted, cannot be delegitimized by moral attack — only altered through amendment procedures they typically influence. They bear the arrangement's discipline too (they must amend rather than decree), but they can leave office; the insulation accrues to the office's acts while the burden of valid-but-unjust measures falls on those subject to them, not on the departing officeholder.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, incumbent_constitutional_officeholders, beneficiary,
    powerful, biographical, arbitrage, national).

% Contract, lend, and invest against the assumption that duly enacted constitutional provisions are settled until formally amended. Determinate validity is the substrate of their planning; a regime in which validity could be reopened by moral argument would reprice every long-term commitment. They cannot exit the legal order whose determinacy they rely on.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, commercial_reliance_interests, beneficiary,
    organized, immediate, constrained, national).

% Live under constitutional provisions that were duly enacted and that bear on them unjustly — votes diluted, status diminished, claims discounted. The pedigree rule answers every moral appeal they make with the same reply: the provision was properly enacted; the remedy is amendment. The amendment procedures that could relieve them demand supermajorities and institutional access they do not have, and leaving the jurisdiction is beyond most of them. Their moral-content arguments are categorically out of order rather than weighed and rejected.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, subjects_of_valid_unjust_provisions, payer,
    powerless, biographical, trapped, national).

% Argue that extreme injustice defeats legal validity and that moral principles number among the sources of law. Their arguments are not answered within the validity conversation — they are ruled out of order by the arrangement's structure, which is precisely the boundary the arrangement maintains. They continue theorizing in academies, and in some jurisdictions their position is adopted after crises (as with the post-war repudiation of 'law is law' defenses), but they hold no procedural seat in the validity machinery.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, natural_law_jurists, excluded,
    moderate, generational, mobile, continental).

% Map what the validity arrangement is and does — its social sources, its costs, its failure modes — from outside the practice. They document the arrangement's dependence on continuing official acceptance, its historical collapses under moral pressure, and the widening gap between the strong separation thesis and actual adjudicative practice.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, analytical_jurisprudence_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text_authority__positivist_reading, incumbent_constitutional_officeholders).
narrative_ontology:fixing_cost_class(constitutional_text_authority__positivist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates thousands of officials, judges, and lawyers on a single shared criterion of constitutional validity — enactment pedigree — so that the question 'is this provision law?' has one institutional answer, enabling planning, orderly succession of rules, and resolution of official disagreement without each official adjudicating moral merit.
% TRANSFER_FUNCTION: Moves the practical power of constitutional change away from courts weighing moral content (and the diffuse claimants who would persuade them) and toward supermajoritarian amendment procedures controlled by institutionally positioned actors; and, for as long as enactment pedigree holds, moves the cost of valid-but-unjust provisions onto the minorities who live under them.
% ABSENT_VOICES: Natural-law jurists and the subjects of valid-but-unjust provisions would object that extreme injustice bears on validity; they are present in public argument but absent from the validity conversation itself, where their claims are categorically out of order rather than heard and rejected. The unanimity of the arrangement's official voice is partly produced by this exclusion.
% DISAPPEARANCE_RATIONALE: If pedigree-based validity vanished overnight, the status of every constitutional provision would become contestable on moral grounds at once; courts would need a replacement criterion — historical understanding, present moral principle, or nothing — long-term planning built on determinate validity would reprice, and the professional machinery organized around enactment records would lose its object. The world would not stay the same.
% FOUNDING_PROBLEM: In pluralistic societies with deep moral disagreement, a validity criterion that turned on moral merit would make legal validity indeterminate, hand unaccountable discretion to whoever adjudicated the morality, and leave officials with no shared answer to 'what counts as law here?' The arrangement was built to supply a shared, institutional criterion of validity that does not depend on contested moral premises.
% FOUNDING_PROBLEM_CORROBORATION: The rival readings' own theorists corroborate that the founding problem is live even while rejecting this solution: living-constitutionalist jurists build doctrines of manageable judicial standards precisely because the indeterminacy worry is real, and originalists cite unaccountable judicial moralizing as the problem their reading answers. Historical scholarship on pre-positivist validity disputes and on the post-war reckoning with 'law is law' defenses corroborates both the problem's reality and the arrangement's failure modes. No corroborating source outside the beneficiary set attests that pedigree validity is the only or best answer to the problem.
narrative_ontology:disappearance_verdict(constitutional_text_authority__positivist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text_authority__positivist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text_authority__positivist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_text_authority__positivist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text_authority__positivist_reading, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text_authority__positivist_reading_tests).
:- end_tests(constitutional_text_authority__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.38: from this reading's own seat, the arrangement's costs beyond the price of its coordination function are real but bounded — the burden falls on those who live under duly enacted provisions that treat them unjustly and cannot reach amendment, and on moral-content claimants whose arguments are categorically out of order. A committed positivist does not deny these costs but counts most of the total as the designed price of determinate validity, with the residue (incumbent insulation beyond what determinacy requires, amendment channels hardened by entrenchment) as the genuinely extractive remainder. Suppression is 0.51, authored as a raw structural property (unscaled — only extractiveness is scaled by directionality and scope in the engine's computation): the arrangement does not coerce persons, it excludes a class of argument — doctrinally, procedurally, and through professional formation — and the exclusion must be actively maintained, which is why the enforcement-history series is tracked here. Theater is 0.46 at interval end and rising: the validity machinery still works (enactment records are still read, amendment is still the channel), but a growing share of maintenance is ritual restatement of the strong separation thesis in textbooks and opinions while actual adjudication increasingly runs on morally inflected doctrine. Accessibility_collapse is 0.48: understanding the arrangement does not collapse the alternatives — rival criteria of validity remain fully articulable and are held by live schools; the arrangement governs operative practice, not the conceptual space. Resistance is 0.55, sustained across the interval: the post-war repudiation of 'law is law' defenses, the Radbruch formula, the Dworkinian attack, and rights-based constitutionalism each forced the arrangement to re-found itself rather than collapse. All three series run on one shared time grid so every tracked metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat and the administrator/beneficiary seats should compute different types from the same structure. From the apex-court and profession seats, the arrangement is the condition of their authority: determinate validity is what makes a final answer possible and what makes legal expertise technical rather than prophetic. From the seat of those living under valid-but-unjust provisions, the same arrangement is a foreclosure: every moral appeal returns the same institutional answer — duly enacted, remedy is amendment — and the amendment channel demands supermajorities they cannot assemble. Coalition through the amendment channel is the designed remedy, but it requires exactly the institutional access the payer seat lacks. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for the legal profession (collects professional authority without running the machinery), incumbent officeholders (collect insulation while able to exit office — the arbitrage position), and commercial reliance interests (collect determinacy). The victim declaration drives high directionality for subjects of valid-unjust provisions: trapped exit and powerless power put them near the full-target end, and the trap is structural — amendment supermajorities and jurisdictional immobility. Apex courts sit dual: they administer the test and collect legitimacy-protection from it, but are also bound by it (they may not do moral lawmaking), which the secondary beneficiary role records. Natural-law jurists sit outside the flow of gains and burdens entirely — the arrangement's enforcement object is the exclusion of their arguments, not any transfer from them — so they carry no beneficiary or victim declaration and fall to the canonical midpoint, which is the honest reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — indeterminacy and unaccountable discretion under a morality-based validity criterion — is live, corroborated by the rival readings' own concessions that determinacy is a real value; status=live crossed with world_rearranges yields no zombie flag. The mandatrophy risk runs in both directions. Reading the arrangement as pure extraction (the temptation from the payer seat) would mislabel a functioning rule of recognition and erase the coordination every legal order needs. Reading it as pure coordination (the profession's self-description) would erase the asymmetric burden the measurements track. The forward risk is inertial drift: the strong separation thesis is increasingly maintained theatrically while practice runs on morally inflected doctrine; if the procedural core were to atrophy — entrenchment hardening amendment beyond reach — the arrangement would persist as ritual over a closed channel and theater_ratio would cross 0.5. It has not yet.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is the positivist_reading of the kernel constitutional_text_authority, not the kernel itself: is the structural picture authored here (beneficiaries, payers, epsilon over the pedigree-validity arrangement) stable as THIS reading''s constraint, or does it silently import contest content that belongs to the sibling readings?',
    'Compare the three reading-stories'' structural data: each should author its own epsilon, its own beneficiary/victim sets, and its own claimed_type over its own instantiation; convergence on identical structures would indicate the kernel, not a reading, was authored.',
    'If the sibling readings relocate the extraction (e.g. the living-constitutionalist reading authors the exclusion of moral content as its central cost over the same referent), cross-reading aggregation of this story''s epsilon is invalid and the kernel-level verdict must be assembled per-reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexicality of the authored structure within the constitutional_text_authority kernel.').

omega_variable(
    inclusive_exclusive_positivism_boundary,
    'Does this reading''s framework permit moral criteria to enter validity through the rule of recognition itself (inclusive positivism), or is the separation absolute (exclusive)? The forecloses edge to the living-constitutionalist reading is strict only on the exclusive side.',
    'Doctrinal and theoretical analysis: if a recognition rule stated in social-fact terms can incorporate moral criteria without ceasing to be a pedigree rule, living-constitutionalist practice is absorbable and the edge softens toward coexists_with; if not, the contradiction is strict.',
    'Inclusive resolution lowers this reading''s suppression and epsilon (moral content becomes absorbable rather than foreclosed) and weakens the foreclosure; exclusive resolution keeps the edge strict and raises the categorical-exclusion cost borne by moral claimants.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inclusive_exclusive_positivism_boundary, conceptual, 'Whether the reading''s separation thesis is exclusive or absorbably inclusive.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the maintenance of the law/morality distinction structural (doctrinal exclusion, procedural bars, admissibility rules) or internalized (professional formation that trains lawyers and judges to experience moral-content reasoning as categorically incompetent)?',
    'Observe jurisdictions and practices where the doctrinal bar is lifted (dignity-clause and proportionality jurisdictions): if pedigree-only reflexes persist among professionally formed lawyers there, the internalized share is substantial.',
    'If internalized, the arrangement''s effective suppression persists after formal rules change — professional socialization carries the distinction — and reform that touches only doctrine underestimates the enforcement still operating.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized mechanism of the distinction''s maintenance.').

omega_variable(
    entrenchment_extraction_trajectory,
    'Is the late-interval rise in extractiveness (t60-t80) driven by intensifying amendment entrenchment that lengthens the burden of valid-but-unjust provisions, and does the rise continue?',
    'Comparative constitutional data on amendment-rigidity trends and on the duration of minority burdens under duly enacted unjust provisions before relief.',
    'A continuing rise pushes the payer seat toward the full-target end and the arrangement toward snare-flavored per-seat classification for trapped payers; a plateau supports the tangled_rope reading with bounded extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(entrenchment_extraction_trajectory, empirical, 'Whether entrenchment hardening drives the extraction re-accumulation.').

omega_variable(
    conceptual_necessity_vs_constructed_arrangement,
    'Is pedigree-based validity a conceptual necessity of law — as the reading''s analytical wing sometimes claims, a truth about what law is — or a constructed institutional arrangement whose persistence benefits identifiable professional and incumbent seats?',
    'Cross-jurisdictional and historical comparison: legal orders that operate validity on other grounds (customary, religious, rights-embedded validity) without collapse would show the pedigree test is contingent rather than conceptually required.',
    'If conceptually necessary, the arrangement approaches a natural-feature reading and resistance reflects confusion; if constructed, the analytical-wing necessity claim functions as cover for the seats that benefit, and the contested-arrangement reading stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conceptual_necessity_vs_constructed_arrangement, conceptual, 'Whether the validity arrangement is a conceptual necessity or a constructed, beneficiary-bearing arrangement.').

omega_variable(
    amendment_burden_price_or_extraction,
    'Is the burden borne by subjects of valid-but-unjust provisions — law they cannot morally challenge and cannot amend — the legitimate price of democratic proceduralism, or extraction through the procedural channel?',
    'Not resolvable by data alone: it turns on the relative weight assigned to democratic procedural legitimacy versus minority moral claims, a values ordering the analysis records but does not settle.',
    'If price, the payer seat''s effective burden drops toward coordination cost and the arrangement reads closer to pure coordination from every seat; if extraction, the payer seat reads toward full target and the asymmetric-cost face dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_burden_price_or_extraction, preference, 'Whether the amendment-channel burden is coordination price or extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text_authority__positivist_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text_authority__positivist_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(cons_tr_t10, constitutional_text_authority__positivist_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement(cons_tr_t20, constitutional_text_authority__positivist_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(cons_tr_t30, constitutional_text_authority__positivist_reading, theater_ratio, 30, 0.25).
narrative_ontology:measurement(cons_tr_t40, constitutional_text_authority__positivist_reading, theater_ratio, 40, 0.29).
narrative_ontology:measurement(cons_tr_t50, constitutional_text_authority__positivist_reading, theater_ratio, 50, 0.33).
narrative_ontology:measurement(cons_tr_t60, constitutional_text_authority__positivist_reading, theater_ratio, 60, 0.37).
narrative_ontology:measurement(cons_tr_t70, constitutional_text_authority__positivist_reading, theater_ratio, 70, 0.42).
narrative_ontology:measurement(cons_tr_t80, constitutional_text_authority__positivist_reading, theater_ratio, 80, 0.46).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text_authority__positivist_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement(cons_be_t10, constitutional_text_authority__positivist_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(cons_be_t20, constitutional_text_authority__positivist_reading, base_extractiveness, 20, 0.37).
narrative_ontology:measurement(cons_be_t30, constitutional_text_authority__positivist_reading, base_extractiveness, 30, 0.36).
narrative_ontology:measurement(cons_be_t40, constitutional_text_authority__positivist_reading, base_extractiveness, 40, 0.36).
narrative_ontology:measurement(cons_be_t50, constitutional_text_authority__positivist_reading, base_extractiveness, 50, 0.35).
narrative_ontology:measurement(cons_be_t60, constitutional_text_authority__positivist_reading, base_extractiveness, 60, 0.36).
narrative_ontology:measurement(cons_be_t70, constitutional_text_authority__positivist_reading, base_extractiveness, 70, 0.37).
narrative_ontology:measurement(cons_be_t80, constitutional_text_authority__positivist_reading, base_extractiveness, 80, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text_authority__positivist_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(cons_su_t10, constitutional_text_authority__positivist_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement(cons_su_t20, constitutional_text_authority__positivist_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(cons_su_t30, constitutional_text_authority__positivist_reading, suppression_requirement, 30, 0.59).
narrative_ontology:measurement(cons_su_t40, constitutional_text_authority__positivist_reading, suppression_requirement, 40, 0.61).
narrative_ontology:measurement(cons_su_t50, constitutional_text_authority__positivist_reading, suppression_requirement, 50, 0.53).
narrative_ontology:measurement(cons_su_t60, constitutional_text_authority__positivist_reading, suppression_requirement, 60, 0.5).
narrative_ontology:measurement(cons_su_t70, constitutional_text_authority__positivist_reading, suppression_requirement, 70, 0.54).
narrative_ontology:measurement(cons_su_t80, constitutional_text_authority__positivist_reading, suppression_requirement, 80, 0.51).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text_authority__positivist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_text_authority__positivist_reading, constitutional_text_authority__originalist_reading).
narrative_ontology:affects_constraint(constitutional_text_authority__positivist_reading, constitutional_text_authority__living_constitutionalist_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'constitutional text authority' covers three structurally distinct claims about the source of the text's authority; per the epsilon-invariance principle they are three constraints, not one constraint with a measurement parameter. This story carries the positivist instantiation: validity from enactment pedigree, law/morality separation maintained, epsilon authored for the pedigree-validity arrangement from this reading's own seat. The originalist instantiation shares this reading's text-fidelity but re-grounds authority in historical public understanding — this reading supplies the validity grounding that reading's interpretive method presupposes (an influences edge). The living-constitutionalist instantiation asserts the direct contrary source claim (contemporary moral principles), which this reading's foundational axiom rules out within a single framework (a forecloses edge). Cross-reading epsilon comparison is reading-indexed and must not be averaged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
