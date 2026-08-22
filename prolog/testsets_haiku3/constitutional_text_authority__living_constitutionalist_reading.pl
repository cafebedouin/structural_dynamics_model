% ============================================================================
% CONSTRAINT STORY: constitutional_text_authority__living_constitutionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text_authority__living_constitutionalist_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: constitutional_text_authority__living_constitutionalist_reading
 *   human_readable: Constitutional Meaning Authority — Living Constitutionalist Reading
 *   domain: legal/constitutional/interpretive
 *
 * SUMMARY:
 *   This constraint instantiates the living constitutionalist reading of
 *   constitutional text authority. The reading holds that constitutional
 *   meaning evolves as social attitudes and moral understanding develop; the
 *   Constitution's authority derives from both its ancient text and the
 *   contemporary moral principles judges apply to changing circumstances.
 *   This is a READING of the contested kernel 'constitutional text
 *   authority'—one interpretive framework judges use to adjudicate what the
 *   Constitution permits and forbids. The constraint story models this
 *   reading's structural effects on judicial practice, which text-based
 *   alternatives (originalism, positivism) cannot accommodate: unenumerated
 *   rights recognition, substantive-due-process flexibility, and adaptation
 *   without formal amendment (exemplified by Brown v. Board, 1954, which
 *   applied the Fourteenth Amendment's guarantee of equal protection to
 *   overturn the previous reading that permitted separate-but-equal
 *   segregation). The reading functions as both coordination (provides a
 *   framework for resolving constitutional disputes across time and changing
 *   social facts) and extraction (concentrates interpretive authority in
 *   judges and devalues textual constraint and historical meaning as binding
 *   limits). The claim/metric divergence is structural: the reading is
 *   claimed as tangled rope (coordination + enforcement + beneficiaries)
 *   while measuring substantial extraction and rising theater ratio—the
 *   pattern of a constraint that began with genuine coordination function but
 *   drifted toward gatekeeping.
 *
 * KEY AGENTS:
 *   - judiciary_adaptive_authority: Supreme Court majority reading the Constitution as living document; holds power to declare precedent overruled and reinterpret constitutional scope without amendment; benefits from authority to adapt; faces rising resistance
 *   - originalist_legal_tradition: judges, scholars, and organized conservative jurisprudence maintaining text-and-history reading; suppressed by living constitutionalist judicial majority; subject to gatekeeping when minority; bears cost of losing canonical-text authority
 *   - constitutional_stability_defenders: actors preferring amendment over judicial reinterpretation; want formal procedure to constrain judicial discretion; lose legitimacy when courts bypass amendment to achieve same outcomes
 *   - contemporary_justice_movements: social movements (civil rights, LGBTQ+ equality, reproductive autonomy) that benefit from judges' willingness to extend constitutional protection to unenumerated or disfavored claims; coordinate with living constitutionalist framework
 *   - positivist_legal_tradition: judges and scholars maintaining law/morality distinction and formal-source-only validity; suppressed by living constitutionalism's moral-progress framing; lose analytical purchase when constitutionalism becomes value-infused
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text_authority__living_constitutionalist_reading, 0.62).
domain_priors:suppression_score(constitutional_text_authority__living_constitutionalist_reading, 0.48).
domain_priors:theater_ratio(constitutional_text_authority__living_constitutionalist_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text_authority__living_constitutionalist_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text_authority__living_constitutionalist_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(constitutional_text_authority__living_constitutionalist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text_authority__living_constitutionalist_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text_authority__living_constitutionalist_reading, "Constitutional Meaning Authority — Living Constitutionalist Reading").
narrative_ontology:topic_domain(constitutional_text_authority__living_constitutionalist_reading, "legal/constitutional/interpretive").

domain_priors:requires_active_enforcement(constitutional_text_authority__living_constitutionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text_authority__living_constitutionalist_reading, '5c0373f4-0ea1-4259-960c-898f0dfff042').
narrative_ontology:cs_kernel_codification('5c0373f4-0ea1-4259-960c-898f0dfff042', fixed_text).
narrative_ontology:cs_authority_grounding('5c0373f4-0ea1-4259-960c-898f0dfff042', lineage).
narrative_ontology:cs_interpretation_layer_present('5c0373f4-0ea1-4259-960c-898f0dfff042').
narrative_ontology:cs_reading_relation('5c0373f4-0ea1-4259-960c-898f0dfff042', constitutional_text_authority__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('5c0373f4-0ea1-4259-960c-898f0dfff042', constitutional_text_authority__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('5c0373f4-0ea1-4259-960c-898f0dfff042', foundational, constitutional_meaning_evolves_with_values).
narrative_ontology:cs_axiom_status(constitutional_meaning_evolves_with_values, holdable).
narrative_ontology:cs_axiom_grounding('5c0373f4-0ea1-4259-960c-898f0dfff042', constitutional_meaning_evolves_with_values, deontological).
narrative_ontology:cs_axiom('5c0373f4-0ea1-4259-960c-898f0dfff042', foundational, contemporary_morality_is_legitimate_interpretive_resource).
narrative_ontology:cs_axiom_status(contemporary_morality_is_legitimate_interpretive_resource, holdable).
narrative_ontology:cs_axiom_grounding('5c0373f4-0ea1-4259-960c-898f0dfff042', contemporary_morality_is_legitimate_interpretive_resource, empirically_contingent).
narrative_ontology:cs_reference_frame('5c0373f4-0ea1-4259-960c-898f0dfff042', flexible_constitutional_authority).
narrative_ontology:cs_drift_state('5c0373f4-0ea1-4259-960c-898f0dfff042', contemporary_originalist_reaction, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5c0373f4-0ea1-4259-960c-898f0dfff042', '').
narrative_ontology:cs_kernel_id(constitutional_text_authority__living_constitutionalist_reading, constitutional_text_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text_authority__living_constitutionalist_reading, judiciary_adaptive_authority).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__living_constitutionalist_reading, contemporary_justice_movements).
narrative_ontology:constraint_victim(constitutional_text_authority__living_constitutionalist_reading, constitutional_stability_defenders).
narrative_ontology:constraint_victim(constitutional_text_authority__living_constitutionalist_reading, originalist_legal_tradition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(constitutional_text_authority__living_constitutionalist_reading, positivist_legal_tradition).
narrative_ontology:constraint_vindicates(constitutional_text_authority__living_constitutionalist_reading, evolutionary_jurisprudence).
narrative_ontology:constraint_vindicates(constitutional_text_authority__living_constitutionalist_reading, moral_progress_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Supreme Court majority interprets the Constitution as a living document capable of evolving meaning. Sets the framework within which constitutional disputes are resolved; declares what the Constitution permits and forbids; has power to overturn precedent and extend constitutional protection to unenumerated rights. Justifies this authority as inherent in Article III judicial power and the Constitution's own flexible language. Benefits from concentrated authority and freedom from textual constraint. Faces rising institutional resistance and originalist scholarly critique. Cannot exit this position without judicial appointments shifting.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, judiciary_adaptive_authority, agenda_setter,
    institutional, generational, trapped, national).

% Social movements (civil rights, LGBTQ+ equality, reproductive autonomy, religious freedom expansion) benefit from judges' willingness to extend constitutional protection to groups or rights not explicitly enumerated in text. Gain access to constitutional protection through judicial interpretation without waiting for amendment. Depend on living constitutionalist judges remaining in majority for this protection. Cannot exit by leaving the constitutional system but could lose protection if judicial composition shifts.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, contemporary_justice_movements, beneficiary,
    organized, generational, constrained, national).

% Judges and scholars committed to interpreting the Constitution according to its historical public meaning at ratification. Suppressed as a minority reading when living constitutionalism dominates judicial majority. Their opinions are gatekept as dissents rather than framings within which majority reasoning occurs. Must authoritatively argue that living constitutionalism exceeds constitutional warrant. Professional identity bound to textual-originalist method: cannot exit without abandoning scholarly coherence. Bear the cost of institutional marginalization and having their methodological approach characterized as constraint on legitimate interpretation rather than as legitimate interpretation itself.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, originalist_legal_tradition, payer,
    institutional, generational, identity_locked, national).

% Political and legal actors (constitutional scholars, federalists, formalists) who prefer constitutional change to occur through formal amendment (Article V) rather than judicial reinterpretation. Believe that allowing judges to change constitutional meaning without amendment permits majority imposition of values without constitutional consensus. Lose institutional voice when courts bypass amendment to achieve outcomes (e.g., Brown, Roe, Obergefell). Can lobby for constitutional amendment or judicial appointments but cannot prevent living constitutionalist interpretation if judges hold majority.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, constitutional_stability_defenders, payer,
    moderate, biographical, constrained, national).

% Judges and scholars maintaining strict distinction between law and morality, grounding constitutional validity in formal enactment procedures and institutional sources rather than moral content. Suppressed as minority reading when living constitutionalism infuses constitutional interpretation with moral reasoning (contemporary moral principles as interpretive key). Professional identity bound to law/morality distinction: cannot exit without losing analytical framework. Bear cost of institutional gatekeeping and characterization of their approach as formalist/mechanical rather than as legitimate legal methodology.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, positivist_legal_tradition, payer,
    institutional, generational, identity_locked, national).

% The eighteenth-century document itself—the shared reference point all three readings claim to interpret. Does not act but is the site of interpretive contest.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, constitutional_text, observer,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(constitutional_text_authority__living_constitutionalist_reading, constitutional_text).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text_authority__living_constitutionalist_reading, judiciary_adaptive_authority).
narrative_ontology:fixing_cost_class(constitutional_text_authority__living_constitutionalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables courts to apply an eighteenth-century document to twenty-first-century circumstances without amendment or wholesale constitutional replacement. Coordinates judicial interpretation across time and changing social facts through flexible meaning rather than rigid text. Solves the genuine problem of constitutional relevance: how to maintain stable law while facts and values evolve.
% TRANSFER_FUNCTION: Transfers interpretive authority from textual constraint and historical meaning to contemporary judicial judgment. Moves authority from the amendment process (where constitutional change requires broad consensus) to the judicial process (where constitutional reinterpretation requires only a 5-justice majority). Moves resources and legal status toward constituencies that benefit from expanded constitutional protection (civil-rights claimants, LGBTQ+ plaintiffs, reproductive-autonomy advocates) and away from constituencies (originalists, stability defenders) whose authority depends on fixed meaning.
% ABSENT_VOICES: Originalist and positivist judges and scholars are not absent but are structurally gatekept as dissenting or marginal voices rather than as framings within which constitutional interpretation occurs. Their absence from the majority's reasoning process is systemic: when they do speak, they must argue that the majority exceeded constitutional warrant rather than offering an alternative constitutional reading within the same framework. The voices truly absent are those without judicial or scholarly platform: persons who would argue for amendment rather than interpretation, or who believe constitutional meaning should be decided by legislative consensus rather than judicial supremacy, have no formal input into constitutional meaning-making.
% DISAPPEARANCE_RATIONALE: If living constitutionalism vanished overnight—courts returning to originalism or positivism or strict textualism—the constitutional landscape would reorganize: privacy rights, unenumerated-substantive-due-process protections, and expansions of equal protection would depend on statutory recognition or amendment rather than judicial interpretation; civil-rights and LGBTQ+-equality frameworks would lose constitutional foundation and revert to statutory/legislative footing; the Court's agenda would shift from rights-expansion to rights-constraint (originalism's typical outputs); the amendment process would become the only reliable path to progressive constitutional change. The constraint is central to how contemporary constitutional law operates.
% FOUNDING_PROBLEM: How can a Constitution written in the eighteenth century, whose amendment requires supermajority consensus, govern a twenty-first-century polity facing social, technological, and moral developments the founders could not have anticipated? How does the document remain authoritative and stable across transformations in social meaning?
% FOUNDING_PROBLEM_CORROBORATION: Living constitutionalists attest the problem is live: courts must interpret constitutional provisions daily in contexts the founders did not contemplate (electronic surveillance, data privacy, reproductive technology, same-sex relationships). Originalists attest the problem was solved by commitment to historical meaning: the Constitution's authors anticipated moral disagreement and created a stable framework by fixing meaning at ratification; courts need not update meaning, only apply it consistently. Positivists attest the problem is not a problem: courts should not interpret meaning at all but should apply constitutional text through formal procedures independent of moral reasoning. Outside the benefiting parties: historical constitutional scholars document that the amendment process has been used sparingly (27 amendments across 236 years), suggesting either the problem is not pressing or the amendment process is too rigid. Legal historians document that judicial reinterpretation has been the actual mechanism of constitutional evolution (both living constitutionalists and originalists agree courts have changed constitutional meaning; they disagree whether courts should have done so). The founding problem's status is genuinely contested because the three readings offer genuinely different solutions, each with institutional power behind it.
narrative_ontology:disappearance_verdict(constitutional_text_authority__living_constitutionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text_authority__living_constitutionalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text_authority__living_constitutionalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_text_authority__living_constitutionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text_authority__living_constitutionalist_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text_authority__living_constitutionalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_text_authority__living_constitutionalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_text_authority__living_constitutionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.62 at interval end) because living constitutionalism concentrates interpretive authority in judges and permits them to override text-based readings without constitutional amendment—the constraint enables extraction of judicial power from democratic amendment processes. The trajectory from 0.38 to 0.62 (40-year series) shows increasing extraction as living constitutionalism expanded from a minority jurisprudential position to the dominant reading, accumulating interpretive authority. Suppression is moderate (0.48) because the constraint's persistence depends on actively suppressing originalist and positivist readings—not by excluding them from jurisprudence entirely, but by institutionalizing living constitutionalism as the framework within which all readings must compete. Originalists and positivists remain present in dissent and scholarship, but their readings are gatekept as subordinate framings. Theater ratio (0.41) reflects that the constraint performs significant legitimacy work: living constitutionalism must be justified as faithful interpretation (not judicial amendment), as grounded in the Constitution's own flexibility (not in judges' values), and as connected to historical constitutional evolution rather than contemporary preference. The theatrical element rises as the judicial majority increasingly must defend expanded authority against originalist critique. Accessibility collapse (0.38) is low because alternatives (originalism, positivism, formalism) remain cognitively and institutional available—courts cannot prevent lower courts or dissenting justices from authoring different readings, nor can they close off law-school instruction in textualist methods. Resistance (0.72) is high because originalist and positivist traditions actively resist living constitutionalism through competing opinions, scholarship, and coordinated constitutional interpretation movements. The measured values reflect the constraint's actual operation: real coordination function (interprets an ambiguous, centuries-old document across changing social facts), real extraction (concentrates authority in judges), required enforcement (suppressing alternative readings), and significant resistance from the suppressed traditions.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's perspective, the constraint is coordination: it permits the Constitution to evolve and remain relevant. From the originalist perspective, it is a snare: judicial authority is exercised without the check of textual meaning or historical constraint. From the contemporary-justice-movements perspective, it is rope: genuine coordination enables access to rights that text-based readings deny. These are not perspectives on the same constraint—they reflect the genuine structural asymmetry living constitutionalism creates. The engine reads them as per-seat classifications computed from beneficiary/victim + power + exit, which is exactly what the structure is.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary's directionality is low (near 0.2) because the judicial seat benefits from living constitutionalism—it concentrates authority, permits flexible responses, and is justified as a reading the Constitution itself permits. No exit is required: judges naturalize the reading as what the Constitution means. Originalist and positivist judges are identity-locked (professional identity as constitutional scholars is bound to their interpretive method; defecting means losing professional coherence), so their exit is constrained. The living constitutionalist reading's beneficiaries (contemporary justice movements, civil-rights constituencies) have directionality near 0.3 (beneficiaries without control—they benefit from judicial outcomes but do not set the interpretive framework). Constitutional stability defenders and originalists have directionality near 0.8 (targets of the constraint—they bear the cost of suppressed readings and constrained alternatives). The beneficiary declaration maps to real structural benefit: judges collect authority, social movements collect expanded constitutional protection. The victim declaration maps to real structural cost: originalists and stability defenders lose the canonical-text limiting principle and must defend their reading as marginal.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem ('what does a centuries-old, politically contested document mean when social facts change and new constituencies invoke it?') is live: courts still face this problem daily. The founding problem status is contested because originalists argue it was solved by commitment to historical meaning, while living constitutionalists argue it requires ongoing adaptive interpretation. The disappearance verdict is world_rearranges: if living constitutionalism vanished and courts returned to originalism or positivism, unenumerated-rights protections (privacy, personal autonomy, equal protection beyond race) would depend on amendment or statutory recognition rather than judicial interpretation—the civil-rights and LGBTQ+-equality landscapes would reorganize. This is not a mandatrophy (a constraint whose function has disappeared but persists through inertia). Living constitutionalism's coordination function is still active—courts still must interpret an old document in a changing world. But the constraint is tangled rope, not rope: the coordination function (enabling adaptive interpretation) and the extraction function (concentrating authority in judges) are operationally inseparable. The extraction is what enables the coordination—the very flexibility that lets courts interpret the Constitution across time also lets judges use contemporary morality rather than historical meaning as the decision rule. Courts could in principle coordinate interpretation through originalist or positivist methods instead, so the extraction is not necessary to the coordination. This is the tangled-rope structure: both elements are present, both are enforced actively, and both ride the same institutional mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_authority_grounding,
    'Does judicial authority to evolve constitutional meaning derive from the Constitution itself (a structural feature of Article III or the Ninth Amendment), or from judges'' claim to contemporary moral philosophy? Is the authority endogenous or imposed?',
    'Textual analysis of constitutional sources claiming to ground living constitutionalism within the document itself versus external jurisprudential claims. Examination of when living constitutionalism arguments appear in Supreme Court opinions: do they cite constitutional text/structure or external moral premises?',
    'If endogenous (structural), the constraint is constitutional coordination with embedded adaptation; if imposed, the constraint is extractive use of judicial position to override text-based limits. Classification could shift toward snare if the authority is entirely extra-constitutional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_authority_grounding, conceptual, 'Whether living constitutionalism''s authority derives from constitutional structure or from judges'' extra-constitutional moral philosophy.').

omega_variable(
    suppression_mechanism_internalized,
    'Is the suppression of originalist and positivist readings structural (institutional gatekeeping by the judicial majority) or internalized (originalists and positivists accept judicial supremacy as legitimate even when they disagree with specific outcomes)?',
    'Post-outcome reaction analysis: originalist and positivist jurists'' expressed acceptance of Supreme Court authority to reinterpret the Constitution versus their persistence in authoring contrary opinions and scholarship. Presence of institutional exit mechanisms (court-packing campaigns, constitutional amendment proposals, judicial appointments reversals) versus acceptance of the supremacy hierarchy.',
    'If internalized, the suppression is lower than measured and represents institutional legitimacy rather than coercive force; if structural, the suppression accurately reflects gatekeeping and the constraint''s extractive pressure on minoritized readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized, empirical, 'Suppression mechanism: institutional gatekeeping versus internalized acceptance.').

omega_variable(
    kernel_reading_versus_constitutional_type,
    'Is this constraint one reading of the contested kernel ''constitutional text authority,'' or is it a claim about what the Constitution structurally IS (a living document)? The reading-frame (what the text means) and the constitutional-type frame (what kind of document it is) can diverge.',
    'Distinguish living constitutionalist arguments grounded in ''the Constitution as written allows adaptive interpretation'' (reading-internal, fits the kernel frame) from arguments grounded in ''the Constitution is inherently a living, evolving instrument'' (constitutional-type claim, not reading-specific). Living constitutionalists use both; this constraint instantiates the former. The latter would require a separate constitutional-type constraint story.',
    'If conflated, this story over-claims the scope of the reading and introduces uncertainty about what is being adjudicated. Keeping them separate preserves ε-invariance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_versus_constitutional_type, conceptual, 'Boundary between reading-of-the-kernel and constitutional-type claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text_authority__living_constitutionalist_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(cons_tr_t0, observed).
narrative_ontology:measurement(cons_tr_t20, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement_basis(cons_tr_t20, observed).
narrative_ontology:measurement(cons_tr_t40, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement_basis(cons_tr_t40, observed).
narrative_ontology:measurement(cons_tr_t60, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 60, 0.41).
narrative_ontology:measurement_basis(cons_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(cons_be_t0, observed).
narrative_ontology:measurement(cons_be_t20, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement_basis(cons_be_t20, observed).
narrative_ontology:measurement(cons_be_t40, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement_basis(cons_be_t40, observed).
narrative_ontology:measurement(cons_be_t60, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 60, 0.62).
narrative_ontology:measurement_basis(cons_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(cons_su_t0, observed).
narrative_ontology:measurement(cons_su_t20, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 20, 0.41).
narrative_ontology:measurement_basis(cons_su_t20, observed).
narrative_ontology:measurement(cons_su_t40, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 40, 0.45).
narrative_ontology:measurement_basis(cons_su_t40, observed).
narrative_ontology:measurement(cons_su_t60, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 60, 0.48).
narrative_ontology:measurement_basis(cons_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text_authority__living_constitutionalist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(constitutional_text_authority__living_constitutionalist_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_text_authority__living_constitutionalist_reading, constitutional_text_authority__originalist_reading).
narrative_ontology:affects_constraint(constitutional_text_authority__living_constitutionalist_reading, constitutional_text_authority__positivist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'constitutional text authority.' The sibling readings (originalist, positivist) instantiate different structural constraints from the same text because they ground authority differently. Living constitutionalism's higher extractiveness reflects concentrated judicial authority and reduced textual constraint; originalism's lower extractiveness reflects commitment to historical meaning as a limiting principle. The three constraints are not different measurements of one constraint—they are different constraints extracted from the same kernel by three interpretive communities. They are linked via network.affects_constraints because living constitutionalism's judicial dominance suppresses originalist and positivist readings, and a shift in any one reading's institutional power would reshape the others' operating environment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_text_authority__living_constitutionalist_reading, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
