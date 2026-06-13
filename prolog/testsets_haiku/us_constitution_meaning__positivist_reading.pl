% ============================================================================
% CONSTRAINT STORY: us_constitution_meaning__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_meaning__positivist_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: us_constitution_meaning__positivist_reading
 *   human_readable: Constitutional Validity via Formal Positivism (Procedural Legitimacy Reading)
 *   domain: constitutional_law/legal_theory/political_philosophy
 *
 * SUMMARY:
 *   Constitutional positivism holds that the U.S. Constitution's validity and
 *   meaning derive from formal enactment procedures and institutional
 *   authority, not from external moral principles. A judge's role, under this
 *   reading, is to interpret and apply the enacted text through procedurally
 *   constrained methods (textual analysis, precedent, historical legislative
 *   intent where germane), and to recognize amendment as the sole legitimate
 *   path to constitutional change. This constraint generates a tangled
 *   coordination-extraction structure: it genuinely solves the coordination
 *   problem of getting agreement on what 'the Constitution says' by anchoring
 *   meaning in procedure rather than subjective moral philosophy. But it
 *   extracts from substantive justice claims that lack textual support, and
 *   it has become increasingly performative—courts claim to follow positivist
 *   procedure while incorporating value judgments through interpretive
 *   choices, theater that masks the constraint's continuing suppression of
 *   moral-philosophical argument in constitutional law.
 *
 * KEY AGENTS:
 *   - institutional_judiciary: Sets and enforces the positivist constraint through doctrine; benefits from procedural legitimacy and insulation from moral-philosophy accusations; constrained to remain within formal method or face delegitimization
 *   - congress_and_amendment_process: Holds exclusive authority to amend; benefits from gatekeeper role; mobile because amendments can occur (though rarely), giving some exit option
 *   - substantive_justice_claimants: Pay the cost of exclusion; seek constitutional protection for novel rights (privacy, reproductive autonomy, gender equality) but lack textual support; constrained because Constitution is supreme law
 *   - marginalized_communities_lacking_textual_recourse: Structurally trapped; historical exclusion from ratification means no textual voice; identity-locked because constitutional membership and legal standing are inseparable
 *   - living_constitutionalists: Excluded from the positivist framework; argue for evolutionary interpretation; structurally incompatible with positivism's prohibition on evolving meaning
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_meaning__positivist_reading, 0.68).
domain_priors:suppression_score(us_constitution_meaning__positivist_reading, 0.72).
domain_priors:theater_ratio(us_constitution_meaning__positivist_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_meaning__positivist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_meaning__positivist_reading, "Constitutional Validity via Formal Positivism (Procedural Legitimacy Reading)").
narrative_ontology:topic_domain(us_constitution_meaning__positivist_reading, "constitutional_law/legal_theory/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_meaning__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_meaning__positivist_reading, 'ab201aa4-a466-4d1e-af8f-34c470deb4d2').
narrative_ontology:cs_kernel_codification('ab201aa4-a466-4d1e-af8f-34c470deb4d2', formalized).
narrative_ontology:cs_authority_grounding('ab201aa4-a466-4d1e-af8f-34c470deb4d2', extraction).
narrative_ontology:cs_interpretation_layer_present('ab201aa4-a466-4d1e-af8f-34c470deb4d2').
narrative_ontology:cs_reading_relation('ab201aa4-a466-4d1e-af8f-34c470deb4d2', us_constitution_meaning__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('ab201aa4-a466-4d1e-af8f-34c470deb4d2', us_constitution_meaning__living_constitutionalist_reading, forecloses).
narrative_ontology:cs_axiom('ab201aa4-a466-4d1e-af8f-34c470deb4d2', foundational, validity_from_procedure_not_morality).
narrative_ontology:cs_axiom_status(validity_from_procedure_not_morality, holdable).
narrative_ontology:cs_axiom_grounding('ab201aa4-a466-4d1e-af8f-34c470deb4d2', validity_from_procedure_not_morality, conventional).
narrative_ontology:cs_axiom('ab201aa4-a466-4d1e-af8f-34c470deb4d2', foundational, amendment_exclusive_legitimacy_path).
narrative_ontology:cs_axiom_status(amendment_exclusive_legitimacy_path, overridden).
narrative_ontology:cs_axiom_grounding('ab201aa4-a466-4d1e-af8f-34c470deb4d2', amendment_exclusive_legitimacy_path, conventional).
narrative_ontology:cs_reference_frame('ab201aa4-a466-4d1e-af8f-34c470deb4d2', procedure_first_constitutional_authority).
narrative_ontology:cs_drift_state('ab201aa4-a466-4d1e-af8f-34c470deb4d2', contemporary_judicial_value_divergence, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ab201aa4-a466-4d1e-af8f-34c470deb4d2', '').
narrative_ontology:cs_kernel_id(us_constitution_meaning__positivist_reading, us_constitution_meaning).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_meaning__positivist_reading, institutional_judiciary).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__positivist_reading, procedural_formalism_tradition).
narrative_ontology:constraint_victim(us_constitution_meaning__positivist_reading, substantive_justice_claimants).
narrative_ontology:constraint_victim(us_constitution_meaning__positivist_reading, marginalized_communities_lacking_textual_recourse).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__positivist_reading, congress_and_amendment_process).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__positivist_reading, originalist_judges_and_scholars).
narrative_ontology:constraint_vindicates(us_constitution_meaning__positivist_reading, legal_positivism).
narrative_ontology:constraint_vindicates(us_constitution_meaning__positivist_reading, separation_of_powers_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_meaning__positivist_reading, amendment_process_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Courts set and enforce the positivist constraint through doctrine and precedent. Judges interpret the Constitution using formal procedures (textual analysis, precedent, legislative history) and claim to exclude moral philosophy from constitutional validity determinations. The judiciary benefits from positivism because it provides institutional legitimacy and insulation from accusations of imposing personal values. Judges are constrained because departing from positivist framing invites institutional delegitimization and challenge.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, institutional_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Congress and state legislatures hold exclusive power to amend the Constitution. Positivism benefits their role by declaring amendment the ONLY legitimate path to constitutional change—no judge may evolve meaning without amendment. This elevates the legislature's authority. However, amendment gridlock means Congress rarely exercises this power, reducing their practical exit option to 'mobile' in theory but constrained in practice.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, congress_and_amendment_process, beneficiary,
    institutional, generational, mobile, national).

% Organized groups seeking constitutional protection for novel claims (privacy rights, reproductive autonomy, gender equality, LGBTQ+ recognition) must find textual support or achieve amendment—both paths are difficult. Positivism forecloses the avenue of arguing that moral principles or evolving social understanding create constitutional rights. These groups pay by having their claims excluded from constitutional discourse unless they can excavate textual support (sometimes through stretching interpretation, which undercuts positivism's claimed procedural neutrality). They are constrained because the Constitution is supreme law and controls their legal standing.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, substantive_justice_claimants, payer,
    organized, biographical, constrained, national).

% Communities whose historical exclusion from ratification processes (enslaved people, women pre-1920, LGBTQ+ communities) left them without textual representation face a structural trap: positivism forecloses moral-philosophical arguments for their inclusion because the Constitution does not speak to their status. Identity-locked because constitutional membership is inseparable from their legal standing and human dignity claims in U.S. law. Exit is unavailable except through amendment, which requires political power these communities have historically lacked. This is the highest-extraction, highest-suppression seat.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, marginalized_communities_lacking_textual_recourse, payer,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_meaning__positivist_reading, marginalized_communities_lacking_textual_recourse, excluded).

% Originalists benefit from positivism's insistence that constitutional meaning is FIXED and not subject to evolving judicial interpretation. Both readings constrain judges and elevate the primacy of the enacted text. However, originalists dispute positivism's complete exclusion of moral reasoning from the *original* public meaning, and their method (historical inquiry) differs from pure positivist procedure. They observe and inhabit the same institutional role as positivist judges, making their exit options equally constrained.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, originalist_judges_and_scholars, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_meaning__positivist_reading, originalist_judges_and_scholars, observer).

% Living constitutionalists argue that constitutional principles endure but their application evolves with social understanding and changing circumstances. Positivism structurally excludes this voice by declaring evolving interpretation illegitimate—validity must flow from formal procedure, not contemporary consensus. Living constitutionalists are institutionally positioned judges and scholars, but the positivist constraint locks them out of the official constitutional reasoning framework, relegating their approach to dissent and academic critique. Constrained by the same institutional role as other judges.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, living_constitutionalist_judges_and_scholars, excluded,
    institutional, generational, constrained, national).

% Moral and natural law traditions ground constitutional legitimacy in principles of justice, human rights, and human dignity—not in formal procedures. Positivism categorically excludes this epistemic authority by declaring moral reasoning outside constitutional law's proper scope. Philosophers have analytical standing to critique positivism but no binding power in courts. They are excluded from the constraint's enforcement mechanisms and can only observe and argue.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, moral_philosophers_and_natural_law_theorists, excluded,
    analytical, civilizational, analytical, global).

% Constitutional scholars document, analyze, and critique the three competing readings. They observe that positivism constrains judicial reasoning by procedure, which benefits institutional legitimacy but excludes substantive justice claims. Scholars have analytical standing and modest influence on judicial thinking through academic publication, but no authority to bind courts. Their role is to track the constraint's operation and provide evidence for omega resolution.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, legal_academy_and_constitutional_scholarship, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_meaning__positivist_reading, institutional_judiciary).
narrative_ontology:fixing_cost_class(us_constitution_meaning__positivist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified, formally predictable method for constitutional interpretation: all participants (judges, legislators, lower courts) follow the same procedural rules (textual interpretation, precedent, legislative history where germane, amendment process as sole path to change). This enables coordination on what 'the Constitution says' rather than fracturing into subjective moral reasoning. Without this coordination, constitutional meaning would diverge across states and federal circuits, destabilizing law.
% TRANSFER_FUNCTION: Moves interpretive authority from substantive moral reasoning (which diverse communities might claim validity for) to formal institutional procedure (controlled by the judiciary and the amendment process). Gains flow to institutional legitimacy and procedural formalism; costs flow to substantive justice claimants, especially historically marginalized communities whose exclusion from ratification means no textual voice.
% ABSENT_VOICES: Moral philosophers, natural law theorists, and substantive-justice advocates arguing that constitutional meaning should evolve with moral understanding. Historically marginalized communities (descendants of enslaved people, women pre-suffrage, LGBTQ+ individuals) whose exclusion from ratification processes left them with no textual recourse and who would argue for moral/justice-based interpretation. These voices are structurally barred from constitutional law's official reasoning framework.
% DISAPPEARANCE_RATIONALE: If positivism disappeared—if judges were permitted to ground constitutional validity in moral reasoning and evolving social understanding—constitutional meaning would become fluid. Rights claims could be recognized without textual support. The amendment process would lose its exclusive gatekeeping role. Institutional legitimacy claims based on procedural neutrality would collapse, exposing value disputes. The constitutional governance structure depends on positivism's constraint that only procedure + text + amendment grant authority.
% FOUNDING_PROBLEM: How can a Constitution drafted in 1787 legitimately govern modern society? How can judges reach agreement on constitutional meaning without subjective moral philosophy driving different outcomes? Positivism's answer: tie validity to formal procedure (text, precedent, amendment) rather than moral philosophy, so legitimacy becomes institutional and predictable, not dependent on the judge's personal values or contemporary moral sentiment.
% FOUNDING_PROBLEM_CORROBORATION: Positivist scholars (H.L.A. Hart, Randy Barnett, and contemporary legal formalists) attest the founding problem remains live—unconstrained moral reasoning produces judicial inconsistency and threatens constitutional legitimacy. Living constitutionalists counter that the problem is solved differently: morality-infused interpretation IS legitimate and better reflects constitutional purpose. Originalists claim procedure alone (historical meaning) solves it. Empirical legal scholarship on judicial polarization documents that judges employing positivist procedure still diverge sharply on constitutional outcomes, suggesting the procedure does not fully solve the legitimacy problem as promised—this external evidence is neither positivist nor anti-positivist, but shows the founding problem remains contested even with positivism in place.
narrative_ontology:disappearance_verdict(us_constitution_meaning__positivist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_meaning__positivist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_meaning__positivist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(us_constitution_meaning__positivist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_meaning__positivist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_meaning__positivist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_meaning__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.68) because the constraint continuously excludes substantive justice arguments from constitutional validity determinations, and the excluded arguments often come from powerless communities (marginalized_communities_lacking_textual_recourse). The exclusion is not temporary—it is structural to the positivist reading itself. Suppression is higher (0.72) because maintaining the constraint requires active exclusion: courts must reject moral-philosophical arguments as outside constitutional law's proper scope, even when those arguments are compelling. This suppression is enforced through doctrine, legal training, and institutional norms that define what counts as 'constitutional reasoning.' Theater ratio is moderate-high (0.48) and rising slightly over the interval because judges increasingly incorporate substantive values through interpretive choices while maintaining the positivist facade—the formally neutral procedure obscures value-laden outcomes, which is theatrical maintenance of positivism's legitimacy claim. Accessibility_collapse (0.71) is high because once a community understands that positivism forecloses moral-philosophical claims, exit becomes cognitively unavailable: marginalized communities cannot simply leave or reframe their claims without abandoning constitutional standing altogether. Resistance (0.59) is real—legal academy, living constitutionalists, and moral philosophers steadily argue against positivism—but the constraint persists because institutional incentives favor procedural formalism.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional_judiciary's seat, positivism is legitimate coordination: a shared framework that produces predictability and insulates judges from accusations of imposing personal values. From the marginal_communities' seat, the same framework is a mechanism of exclusion that weaponizes the Constitution's 18th-century text against modern justice claims. The judiciary experiences constraint (they must follow procedure); the excluded communities experience suppression (they cannot access the constitutional forum at all). This perpectival divergence is core to the tangled_rope classification: real coordination benefit exists for institutional actors and the amendment process, while extraction and suppression fall on those without textual recourse.
 *
 * DIRECTIONALITY LOGIC:
 *   The institutional_judiciary sits near the beneficiary end (d ~ 0.20–0.30): they gain procedural authority, insulation from personal-philosophy accusations, and institutional legitimacy. Congress/amendment_process sits near symmetric (d ~ 0.45–0.55): they coordinate on a shared framework but also benefit from the gatekeeper role, creating modest extraction on their side. Substantive_justice_claimants and marginalized_communities sit near the target end (d ~ 0.75–0.90): they bear the cost of exclusion most heavily. Originalists sit near beneficiary (d ~ 0.25): they gain constraint on judicial updating, though they disagree on method. Living constitutionalists and moral philosophers would experience the constraint as pure extraction (d ~ 0.95 for excluded voices), but they are not seated as stakeholders here—they are named as excluded to document the constraint's suppression of their participation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—how to establish constitutional legitimacy without tying it to subjective moral reasoning—remains CONTESTED after 200+ years. No party has abandoned the problem; all three readings (positivist, originalist, living constitutionalist) claim to solve it. The positivist reading's mandated function is to enable coordination through procedure. But empirical judicial behavior shows that positivist procedure does not eliminate value disputes—judges employing positivist method still diverge on outcomes, and empirical legal scholarship documents that rhetorical procedure-following often masks substantive value choices. This suggests the founding problem is NOT solved by positivism alone; the constraint persists by institutional inertia and benefit to the judiciary, not because it fully solves the legitimacy question. Mandatrophy is NOT yet resolved (the founding problem is live), but the theater-ratio rise indicates performative maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    procedure_masking_values,
    'Does positivist procedure genuinely constrain judicial reasoning, or does it merely provide a neutral-sounding facade for value-laden interpretive choices?',
    'Empirical study of judicial voting patterns and written opinions comparing positivist-method judges with living-constitutionalist judges: do positivist judges show less value divergence or more consistent outcomes? Do they explicitly acknowledge value tradeoffs or suppress them in rhetoric?',
    'If procedure masks values, the theater_ratio is artificially high and suppression is more performative than structural. If procedure genuinely constrains, extraction is lower than authored. Resolution would clarify whether the constraint is snare (pure extraction under a coordination cover) or genuinely tangled_rope (real coordination benefit with asymmetric cost).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedure_masking_values, empirical, 'Whether formal procedure in constitutional interpretation constrains or merely rhetorically frames judicial value choices.').

omega_variable(
    textual_bias_as_structural_extraction,
    'Is the requirement that constitutional claims find textual support a neutral procedural rule, or does it structurally favor the historical majority whose views are inscribed in the founding text?',
    'Comparative analysis: count the proportion of successful constitutional claims by the historical in-group (e.g., property-owning white males) vs. historically excluded groups (enslaved people, women, LGBTQ+) and test whether textual-support requirement explains the gap.',
    'If textual bias is structural, the constraint extracts systematically from marginalized communities. This would support reclassification toward snare (the coordination function is real but secondary to the extraction function) and identify the true victim class more precisely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_bias_as_structural_extraction, empirical, 'Whether the textual-support requirement for constitutional claims systematically advantages historical majorities.').

omega_variable(
    amendment_gridlock_collapse,
    'When the amendment process gridlocks, does positivism collapse into originalism in practice, making the distinction between readings merely institutional rather than structural?',
    'Historical analysis: under gridlock, do positivist judges explicitly adopt originalist reasoning (historical public meaning) as a substitute for amendment, effectively merging the two readings?',
    'If positivism collapses into originalism under gridlock, the two readings are not independent constraints but one constraint with a fallback mode. This would suggest the structural delta between readings is smaller than anticipated and that the committer contest is narrower than the three-way framing suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_gridlock_collapse, conceptual, 'Whether positivism''s procedural framework remains distinct from originalism when amendment is infeasible.').

omega_variable(
    moral_reasoning_suppression_internalized,
    'For legal actors trained in positivism (law students, judges), is the exclusion of moral reasoning from constitutional law structurally enforced or internalized as a professional norm?',
    'Sociological study of legal education and judicial socialization: are positivist judges constrained by external enforcement or by internalized professional identity that makes moral reasoning feel illegitimate?',
    'If internalized, the suppression persists even without active institutional enforcement—judges carry it with them. This would elevate the effective suppression above the structural level and suggest identity-lock dynamics for judicial actors. If structural, removal of the formal constraint would change behavior immediately.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_reasoning_suppression_internalized, empirical, 'Whether suppression of moral reasoning in constitutional law is structurally enforced or internalized by legal professionals.').

omega_variable(
    alternative_readings_sibling_status,
    'Are originalism and living constitutionalism genuinely distinct readings of the same kernel, or are they alternate kernels altogether?',
    'Textual and historical analysis: do all three readings claim to ground constitutional authority in the same founding text and institutional framework, or do living constitutionalists invoke a different source of authority (contemporary moral consensus)?',
    'If they are distinct kernels, this constraint story should be decomposed and the network should show no affiliation. If they are readings of one kernel, the committer relations (coexists_with, influences) are correctly stated. This affects whether the engine treats them as constraint family or as unrelated constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_readings_sibling_status, conceptual, 'Whether originalism, living constitutionalism, and positivism are competing readings of one kernel or structurally separate kernels.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_meaning__positivist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_meaning__positivist_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(us_c_tr_t0, observed).
narrative_ontology:measurement(us_c_tr_t5, us_constitution_meaning__positivist_reading, theater_ratio, 5, 0.37).
narrative_ontology:measurement_basis(us_c_tr_t5, observed).
narrative_ontology:measurement(us_c_tr_t10, us_constitution_meaning__positivist_reading, theater_ratio, 10, 0.4).
narrative_ontology:measurement_basis(us_c_tr_t10, observed).
narrative_ontology:measurement(us_c_tr_t15, us_constitution_meaning__positivist_reading, theater_ratio, 15, 0.42).
narrative_ontology:measurement_basis(us_c_tr_t15, observed).
narrative_ontology:measurement(us_c_tr_t20, us_constitution_meaning__positivist_reading, theater_ratio, 20, 0.45).
narrative_ontology:measurement_basis(us_c_tr_t20, observed).
narrative_ontology:measurement(us_c_tr_t25, us_constitution_meaning__positivist_reading, theater_ratio, 25, 0.47).
narrative_ontology:measurement_basis(us_c_tr_t25, observed).
narrative_ontology:measurement(us_c_tr_t30, us_constitution_meaning__positivist_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement_basis(us_c_tr_t30, observed).
narrative_ontology:measurement(us_c_tr_t40, us_constitution_meaning__positivist_reading, theater_ratio, 40, 0.48).
narrative_ontology:measurement_basis(us_c_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_meaning__positivist_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(us_c_be_t0, observed).
narrative_ontology:measurement(us_c_be_t5, us_constitution_meaning__positivist_reading, base_extractiveness, 5, 0.61).
narrative_ontology:measurement_basis(us_c_be_t5, observed).
narrative_ontology:measurement(us_c_be_t10, us_constitution_meaning__positivist_reading, base_extractiveness, 10, 0.64).
narrative_ontology:measurement_basis(us_c_be_t10, observed).
narrative_ontology:measurement(us_c_be_t15, us_constitution_meaning__positivist_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement_basis(us_c_be_t15, observed).
narrative_ontology:measurement(us_c_be_t20, us_constitution_meaning__positivist_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(us_c_be_t20, observed).
narrative_ontology:measurement(us_c_be_t25, us_constitution_meaning__positivist_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(us_c_be_t25, observed).
narrative_ontology:measurement(us_c_be_t30, us_constitution_meaning__positivist_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(us_c_be_t30, observed).
narrative_ontology:measurement(us_c_be_t40, us_constitution_meaning__positivist_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(us_c_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_meaning__positivist_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement_basis(us_c_su_t0, observed).
narrative_ontology:measurement(us_c_su_t5, us_constitution_meaning__positivist_reading, suppression_requirement, 5, 0.63).
narrative_ontology:measurement_basis(us_c_su_t5, observed).
narrative_ontology:measurement(us_c_su_t10, us_constitution_meaning__positivist_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(us_c_su_t10, observed).
narrative_ontology:measurement(us_c_su_t15, us_constitution_meaning__positivist_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(us_c_su_t15, observed).
narrative_ontology:measurement(us_c_su_t20, us_constitution_meaning__positivist_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(us_c_su_t20, observed).
narrative_ontology:measurement(us_c_su_t25, us_constitution_meaning__positivist_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(us_c_su_t25, observed).
narrative_ontology:measurement(us_c_su_t30, us_constitution_meaning__positivist_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(us_c_su_t30, observed).
narrative_ontology:measurement(us_c_su_t40, us_constitution_meaning__positivist_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(us_c_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_meaning__positivist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_meaning__positivist_reading, 0.12).
narrative_ontology:affects_constraint(us_constitution_meaning__positivist_reading, us_constitution_meaning__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_meaning__positivist_reading, us_constitution_meaning__living_constitutionalist_reading).

% DUAL FORMULATION NOTE:
% This constraint (positivist reading) is one of three readings of the kernel 'us_constitution_meaning.' Each reading instantiates a different constraint story: originalist (meaning fixed at ratification), living constitutionalist (principles evolve with application), positivist (validity from procedure and institutional authority, not moral philosophy). The three readings coexist across different judicial factions and scholarly traditions. Positivism influences the other readings by constraining the epistemic scope of constitutional reasoning—judges trained in positivism carry its procedure-first approach even when they adopt originalist or living-constitutionalist conclusions. The sibling constraints document the contested kernel; this story documents the positivist reading's structure and extraction profile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_meaning__positivist_reading, powerless, 0.87).
constraint_indexing:directionality_override(us_constitution_meaning__positivist_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
