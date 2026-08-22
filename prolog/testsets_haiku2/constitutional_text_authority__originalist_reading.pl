% ============================================================================
% CONSTRAINT STORY: constitutional_text_authority__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text_authority__originalist_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: constitutional_text_authority__originalist_reading
 *   human_readable: Originalist Constitutional Interpretation Authority
 *   domain: constitutional_law/interpretive_jurisprudence
 *
 * SUMMARY:
 *   The originalist reading of constitutional authority fixes meaning at
 *   ratification, deriving legitimacy from the historical public
 *   understanding of the Framers and ratifying generation. This constraint
 *   instantiates one reading of the contested kernel of constitutional
 *   interpretation authority. The originalist reading competes with living
 *   constitutionalism (meaning evolves with contemporary values) and legal
 *   positivism (validity derives from formal procedures, not moral content).
 *   This story generates the originalist constraint alone: it captures the
 *   specific way originalism structures interpretation, who benefits and who
 *   bears costs under that frame, and the extraction dynamics that arise when
 *   one interpretive method is institutionally dominant. The sibling readings
 *   are separate constraint stories, each with their own ε,
 *   beneficiary/victim structure, and classification.
 *
 * KEY AGENTS:
 *   - originalist_jurisprudential_community: institutional seat, sets the interpretive standard, benefits from authority to adjudicate meaning
 *   - supreme_court_originalist_justices: institutional seat, enforces the constraint through opinions grounding meaning in historical evidence
 *   - living_constitutionalist_judges: institutional seat, bears costs as their preferred doctrine is overturned when originalism dominates
 *   - rights_claimants_without_ratification_basis: powerless seat, trapped — lose constitutional protection when historical basis is unavailable
 *   - social_movements_seeking_constitutional_adaptation: organized seat, constrained — forced to pursue Article V amendment rather than judicial recognition
 *   - legislative_bodies: dual seat — benefit when judicial constraint protects legislative authority, pay when statutes are invalidated on originalist grounds
 *   - historical_evidence_gatekeepers: institutional seat, agenda-setter via expertise — their interpretations of ratification-era meaning gate permissible outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text_authority__originalist_reading, 0.62).
domain_priors:suppression_score(constitutional_text_authority__originalist_reading, 0.58).
domain_priors:theater_ratio(constitutional_text_authority__originalist_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text_authority__originalist_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text_authority__originalist_reading, "Originalist Constitutional Interpretation Authority").
narrative_ontology:topic_domain(constitutional_text_authority__originalist_reading, "constitutional_law/interpretive_jurisprudence").

domain_priors:requires_active_enforcement(constitutional_text_authority__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text_authority__originalist_reading, '7fb59732-b8a4-4f9e-9cc4-88b776e780c1').
narrative_ontology:cs_kernel_codification('7fb59732-b8a4-4f9e-9cc4-88b776e780c1', formalized).
narrative_ontology:cs_authority_grounding('7fb59732-b8a4-4f9e-9cc4-88b776e780c1', lineage).
narrative_ontology:cs_interpretation_layer_present('7fb59732-b8a4-4f9e-9cc4-88b776e780c1').
narrative_ontology:cs_reading_relation('7fb59732-b8a4-4f9e-9cc4-88b776e780c1', constitutional_text_authority__living_constitutionalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('7fb59732-b8a4-4f9e-9cc4-88b776e780c1', constitutional_text_authority__positivist_reading, influences).
narrative_ontology:cs_axiom('7fb59732-b8a4-4f9e-9cc4-88b776e780c1', foundational, meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('7fb59732-b8a4-4f9e-9cc4-88b776e780c1', meaning_fixed_at_ratification, deontological).
narrative_ontology:cs_axiom('7fb59732-b8a4-4f9e-9cc4-88b776e780c1', foundational, historical_public_understanding_governs).
narrative_ontology:cs_axiom_status(historical_public_understanding_governs, holdable).
narrative_ontology:cs_axiom_grounding('7fb59732-b8a4-4f9e-9cc4-88b776e780c1', historical_public_understanding_governs, empirically_contingent).
narrative_ontology:cs_reference_frame('7fb59732-b8a4-4f9e-9cc4-88b776e780c1', historical_public_understanding_authority).
narrative_ontology:cs_drift_state('7fb59732-b8a4-4f9e-9cc4-88b776e780c1', contemporary_living_constitutionalism_challenge, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7fb59732-b8a4-4f9e-9cc4-88b776e780c1', '').
narrative_ontology:cs_kernel_id(constitutional_text_authority__originalist_reading, constitutional_text_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, originalist_jurisprudential_community).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, conservative_political_movements).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, textualist_institutional_actors).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, non_originalist_constitutional_interpreters).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, rights_claimants_without_ratification_basis).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, social_movements_seeking_constitutional_adaptation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, legislative_bodies).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, institutional_continuity_constituency).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, living_constitutionalist_judges).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, legislative_bodies).
narrative_ontology:constraint_vindicates(constitutional_text_authority__originalist_reading, stable_constitutional_meaning_doctrine).
narrative_ontology:constraint_vindicates(constitutional_text_authority__originalist_reading, historical_public_understanding_primacy).
narrative_ontology:constraint_vindicates(constitutional_text_authority__originalist_reading, article_v_amendment_necessity_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legal scholars, constitutional law professors, and intellectual leaders of originalist jurisprudence who develop and defend the originalist interpretive method through scholarship, conferences, and law review articles. They set the intellectual standard for what counts as legitimate constitutional reasoning. They benefit by gaining epistemological authority — originalism is their framework, so its institutional dominance vindicates their scholarly project. They also shape law school curriculum and train the next generation of lawyers and judges in originalist reasoning. Their exit is arbitrage: they could abandon originalism and adopt living constitutionalism or positivism, but that would dissolve their distinctive intellectual identity and their accumulated scholarly authority.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, originalist_jurisprudential_community, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text_authority__originalist_reading, originalist_jurisprudential_community, beneficiary).

% Supreme Court justices who adopt originalist methodology in constitutional decisions. They enforce the constraint by grounding majority and concurring opinions in historical evidence, textual analysis, and original public meaning, and by invalidating statutes and precedents that rest on non-originalist reasoning. They author opinions that require historical accuracy in constitutional arguments and reject arguments based on evolving contemporary standards without clear historical basis. Their exit is constrained: changing their judicial philosophy mid-career would require publicly repudiating their prior work, damaging their professional reputation and legacy. Their career is invested in originalism.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, supreme_court_originalist_justices, agenda_setter,
    institutional, biographical, constrained, national).

% Judges and justices who prefer adaptive constitutional interpretation, where meaning evolves with contemporary moral understanding and social circumstances. They bear costs under the originalist constraint by having to argue their positions in historical-evidence terms even when they disagree with originalism's methodology, by seeing their preferred doctrine overturned when originalist justices gain a court majority, and by having their reasoning attacked as 'activist' or 'unmoored from text' in originalist academic and judicial discourse. They are excluded from setting the dominant interpretive standard when originalist justices control the court. Their exit is constrained: their judicial philosophy is part of their professional identity.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, living_constitutionalist_judges, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text_authority__originalist_reading, living_constitutionalist_judges, excluded).

% Individuals seeking judicial recognition of constitutional rights that lack explicit textual grounding or clear historical-public-understanding support in the ratification era (e.g., privacy rights, dignity-based rights to refuse unwanted medical treatment, rights tied to evolving understanding of personhood and autonomy). Under the originalist constraint, they face systematic judicial rejection of their rights claims because those rights cannot be grounded in 1791 or 1868 understanding. They must either: abandon the constitutional litigation pathway entirely, attempt to reinterpret the historical record in their favor (laborious, epistemically fraught), or pursue Article V amendment (extraordinarily difficult). They cannot exit the constitutional system itself. Their situation is trapped by construction.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, rights_claimants_without_ratification_basis, payer,
    powerless, biographical, trapped, national).

% Organized movements (civil rights movements, LGBTQ+ rights movements, environmental protection movements, reproductive autonomy movements, disability rights movements, immigrant rights movements) that seek constitutional recognition of emerging rights or protections tied to evolving social understanding. Under the originalist constraint, they face a choice: (a) reinterpret the historical record to show the Framers' principles were broad enough to encompass their concerns (difficult and often unconvincing); (b) accept constitutional exclusion and pursue legislative protection at state and federal level (partial, reversible); (c) attempt Article V amendment (extraordinarily difficult — requires 2/3 of both houses plus 3/4 of states). The constraint effectively channels constitutional change away from the judiciary and into the formal amendment process. Movements cannot exit the constitutional system; their exit options are all constrained.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, social_movements_seeking_constitutional_adaptation, payer,
    organized, generational, constrained, national).

% State and federal legislatures that operate under constitutional constraints. They benefit from originalism when it constrains courts from recognizing unenumerated rights, because that preserves legislative authority to make social policy (legislatures retain power to recognize rights through statute rather than having courts declare them unconstitutional). They pay when originalism invalidates statutes on originalist-textualist grounds (e.g., textualist invalidation of regulatory statutes on Commerce Clause or regulatory-authority grounds) or when originalism constrains the statutory meaning of ambiguous laws by refusing purposivist interpretation. Their exit is mobile: they can attempt Article V amendment, engage in legislative override where constitutionally permissible, lobby for judicial appointments that shift the court's composition, or shift political composition to change court.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, legislative_bodies, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text_authority__originalist_reading, legislative_bodies, payer).

% Law professors, judges, and legal advocates who employ non-originalist interpretive methods (living constitutionalism, purposivism, structural interpretation, common-law constitutionalism). They bear costs under the originalist constraint by having to engage originalism's historical-evidence framework even when they believe it is methodologically misguided, by seeing their preferred doctrine restricted or overturned when originalism is institutionally ascendant, by having their methodological legitimacy contested in academic and judicial discourse as 'less rigorous' or 'less faithful to law,' and by experiencing professional marginalization when originalism dominates law schools and courts. Their exit is constrained: their professional identity and career reputation are deeply tied to their interpretive commitments. Abandoning non-originalism for originalism would require publicly repudiating their prior work.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, non_originalist_constitutional_interpreters, payer,
    moderate, biographical, constrained, national).

% Organized groups that would amend the Constitution to recognize new rights or change established meanings without Article V formal amendment. The originalist constraint forces constitutional change into the amendment process (requiring supermajority consensus across states and federal branches), keeping this coalition locked out of the judicial interpretation pathway as a route to constitutional change. Effective power requires Article V amendment — a path that has succeeded only 27 times in over 200 years and has become increasingly difficult as political polarization has risen. Their exit is trapped: they cannot bypass the constitutional amendment process if their goal is constitutional-level change, and they cannot avoid the originalist constraint through any means other than amending the Constitution or changing the Supreme Court's composition.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, constitutional_amendment_coalition, excluded,
    organized, generational, trapped, national).

% The abstract institutional apparatus of constitutional governance (courts, legislatures, executive, federal system) as a continuity-valuing entity, inferred as a beneficiary of the constraint's legitimacy function. The originalist constraint benefits this constituency by providing a rule-governed, apparently apolitical interpretive method that legitimates judicial authority, reduces the appearance of courts as political actors, and creates a coherent standard that survives changes in judicial personnel. This constituency is analytically inferred rather than directly agent; it represents the regime-legitimacy interest in stable, apparently neutral constitutional interpretation. Their situation is described rather than lived; this is the institutional abstraction whose interests the constraint serves.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, institutional_continuity_constituency, beneficiary,
    institutional, civilizational, analytical, national).

% Historians, historical scholars, historical experts, and expert witnesses whose testimony and interpretations of ratification-era evidence determine what meanings are permissible under originalist interpretation. They wield interpretive authority by deciding what the historical public understanding was, what Framer intent was, what the original meaning of contested provisions was. They set evidentiary standards for what counts as valid historical evidence. Their exit is arbitrage: they could decline to participate in constitutional litigation and judicial briefing, but their professional standing, influence, and career advancement depend on participation in the high-stakes constitutional interpretation process.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, historical_evidence_gatekeepers, agenda_setter,
    institutional, generational, arbitrage, national).

% Institutional actors (judges, lawyers, legal scholars) who employ textualist reasoning and advance textualist methodology in law. They benefit from the originalist constraint because originalism's emphasis on textual meaning and linguistic analysis validates their interpretive approach. Originalism and textualism reinforce each other institutionally. Their exit is arbitrage: they could adopt non-textualist approaches, but that would dissolve their distinctive institutional identity.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, textualist_institutional_actors, beneficiary,
    institutional, generational, arbitrage, national).

% Conservative and right-wing political movements that benefit from judicial restraint on recognizing new unenumerated rights, particularly rights tied to sexual autonomy, bodily autonomy, and non-traditional family structures. Under originalism, such rights lack historical ratification-era support and are therefore not judicially recognized. Originalism aligns with conservative political interests by preserving legislative authority to restrict conduct on moral grounds without constitutional impediment. Their exit is mobile: they can attempt to shift court composition through judicial appointments, engage in Article V amendment efforts, or pursue legislative strategies.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, conservative_political_movements, beneficiary,
    organized, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text_authority__originalist_reading, originalist_jurisprudential_community).
narrative_ontology:fixing_cost_class(constitutional_text_authority__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a rule-governed method for constitutional interpretation that constrains judicial discretion within textual and historical bounds, reducing the perception of courts as political actors and creating a coherent, stable interpretive standard that survives changes in judicial personnel. Solves the coordination problem of how judges should construe the same constitutional text consistently across time.
% TRANSFER_FUNCTION: Transfers interpretive authority from contemporary moral reasoning and adaptive judicial judgment to historical evidence and ratification-era public understanding. Moves constitutional legitimacy from judges' reasoning about evolving values to historians' and textualists' claims about past meaning. Moves the locus of constitutional change from the judiciary to the formal amendment process (Article V).
% ABSENT_VOICES: Non-originalist constitutional interpreters are present but delegitimized; rights-claimants without historical ratification basis are structurally excluded from judicial recognition; amendment coalitions seeking constitutional change outside Article V are kept out by the constraint itself; historians offering interpretations of the ratification era that contradict originalist readings are framed as activist rather than authoritative.
% DISAPPEARANCE_RATIONALE: If the originalist constraint on constitutional interpretation disappeared overnight (replaced by, e.g., explicitly living constitutionalism or positivism), the judiciary would immediately recognize a wider range of unenumerated rights, statutes would be invalidated or upheld on different grounds, and the constitutional meaning of many provisions would shift to reflect contemporary understanding. The entire structure of constitutional litigation and doctrine would reorganize. Political coalitions seeking constitutional change would bypass Article V and seek favorable judicial appointments instead. Conservative movements that benefit from originalism's constraint on judicial expansion would lose their institutional guarantee and resort to amendment efforts.
% FOUNDING_PROBLEM: Post-Civil War through mid-20th century jurisprudence allowed judges to read their own policy preferences into the Constitution under the guise of interpretation (Lochner era, substantive due process without textual anchor). The constraint was built to solve the problem of uncontrolled judicial creativity by tethering interpretation to the fixed historical meaning the Framers and ratifying public understood.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars and conservative legal movements attest the founding problem remains live — that without historical constraint, judges still read modern preferences into the text. Non-originalists and civil rights advocates attest the founding problem is either solved (modern proceduralism and scholarly attention have reduced arbitrary interpretation) or was misdiagnosed (judicial review always requires judgment; the problem was never uncontrolled creativity but rather whose values the court enforces). Neither camp has produced consensus from outside the dispute itself; the empirical question of whether Lochner-style overreach persists when originalism is not enforced has not been resolved by neutral parties.
narrative_ontology:disappearance_verdict(constitutional_text_authority__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text_authority__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text_authority__originalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_text_authority__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text_authority__originalist_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text_authority__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_text_authority__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_text_authority__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.62): the constraint transfers interpretive authority from contemporary moral judgment to historical evidence, concentrating power with institutional actors (scholars, judges, historians) who command historical expertise. Rights-claimants without historical basis face effective extraction of constitutional protection. Suppression is significant (0.58) but lower than extractiveness: while the constraint does suppress non-originalist interpretation, that suppression is justified by a real coordination function (stable, rule-governed interpretation). Theater ratio at 0.41 indicates substantial performative activity: originalism presents itself as apolitical constraint discovery when significant portions of contemporary originalist practice involve contentious historical interpretation and selective evidence use (e.g., original public meaning vs. Framer intent debates, Dead Hand Problem arguments). The measurement series tracks the period roughly from the 1990s onward, capturing the rise of originalism's institutional power. Base extractiveness rises from 0.48 to 0.62 over the interval as originalist justices accumulate on the Supreme Court and the interpretive standard becomes more institutionally entrenched, then plateaus. Theater ratio rises similarly as the constraint's performative element (claims of apolitical historical discovery) becomes more central to its legitimacy defense.
 *
 * PERSPECTIVAL GAP:
 *   The originalist jurisprudential community (agenda-setter) experiences the constraint as genuine coordination: a rule-governed method that constrains judicial discretion and stabilizes meaning. From their institutional position, the constraint is a solution to the Lochner problem (uncontrolled judicial creativity). Rights-claimants and social movements (payer seats) experience the same constraint as extraction: the historical-public-understanding gate forecloses constitutional recognition of rights that lack ratification-era textual or historical basis, forcing them to abandon constitutional litigation and pursue Article V amendment (extraordinarily difficult). Living constitutionalist judges (payer/excluded seats) experience the constraint as both suppression and delegitimation: their interpretive methodology is framed as 'activist' and their preferred doctrine is overturned when originalism is institutionally dominant. The engine computes per-seat classification from these structural divergences: originalist justices and the jurisprudential community will likely compute the constraint as rope (genuine coordination with modest asymmetry); rights-claimants and social movements will compute it as snare or tangled_rope (extraction suppressing their options). The perspectival divergence is not a defect — it is the core structure the constraint encodes.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: originalist_jurisprudential_community (directionality near 0.0 — they set the standard, gain institutional authority, control epistemic legitimacy; their interests are advanced by the constraint's persistence), conservative_political_movements (near 0.1 — benefit from judicial restraint on unenumerated rights, but do not directly set the constraint), textualist_institutional_actors (near 0.15 — gain authority when their textual-evidence methodology governs interpretation). Victims: non_originalist_constitutional_interpreters (directionality near 0.8 — their methodology is delegitimized, their preferred doctrine is overturned, their professional authority is contested), rights_claimants_without_ratification_basis (near 1.0 — trapped; lose protection entirely; no exit), social_movements_seeking_constitutional_adaptation (near 0.9 — organized but severely constrained; forced into Article V amendment pathway). Legislative bodies straddle symmetric (d ≈ 0.45) because they benefit from judicial restraint on unenumerated rights but pay when statutes are invalidated on textualist grounds. Historical_evidence_gatekeepers sit near 0.2 (beneficiary-adjacent) because their expertise gains gatekeeping authority, though they do not directly collect extraction. The directionality derivation routes through beneficiary/victim declarations and exit options: beneficiaries with institutional power and arbitrage exit sit near 0.0; victims who are trapped sit near 1.0; organized payers with constrained exit sit in the 0.8–0.9 range.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids misclassification as pure rope by documenting the asymmetric extraction component: rights-claimants and social movements bear substantial costs without corresponding benefit. The constraint also avoids misclassification as pure snare by preserving the genuine coordination function (stable, rule-governed interpretation that constrains judicial discretion and provides interpretive consistency). The tangled_rope classification captures both: there is real coordination (the problem of how to interpret the Constitution consistently is genuinely solved), AND there is asymmetric extraction (the solution distributes costs toward rights-claimants and non-originalist interpreters, while distributing benefits toward institutional originalist actors). The measurement data support this: theater_ratio at 0.41 shows the constraint is not purely theatrical (not piton), but rising theater indicates growing performative element as originalism defends its claim to apolitical constraint discovery against accusations of selective historiography.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_public_understanding_ambiguity,
    'What constitutes ''historical public understanding'' of the Framers and ratifying generation? Are we measuring Framer intent, original public meaning at ratification, the median informed citizen''s understanding, or the understanding of the ratifying state conventions?',
    'Systematic historiographical analysis comparing competing interpretations of specific constitutional provisions (e.g., what ''search and seizure'' meant in 1791) across different methodological approaches. Compare the results of original-public-meaning analysis against Framer-intent analysis and ratification-convention record analysis for a sample of contested provisions.',
    'Different definitions of ''historical public understanding'' can yield radically different originalist conclusions. If the metric shifts from Framer intent to original public meaning to state convention understanding, the same constitutional text could support different permissible meanings. This ambiguity allows originalism to appear determinate when it actually contains a free parameter. Resolving this ambiguity would either stabilize the constraint''s classification or reveal that originalism''s determinacy claim is overstated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_public_understanding_ambiguity, conceptual, 'The semantic content of ''historical public understanding'' is irreducibly contested within originalist methodology itself.').

omega_variable(
    originalism_foreclosure_vs_coexistence,
    'Does originalism logically foreclose living constitutionalism within the same judicial framework, or do the two readings coexist as competing institutional positions held by different justices?',
    'Examine the core axioms: originalism asserts meaning is fixed at ratification; living constitutionalism asserts meaning evolves with contemporary values. These appear contradictory. However, empirically, the U.S. Supreme Court has had originalist and living-constitutionalist justices simultaneously, both claiming constitutional authority. Either: (a) one reading is defeated by the other when they collide (foreclosure), or (b) institutional power determines which reading governs at each moment (coexistence). Determine empirically which: do originalist justices rule living-constitutionalist doctrine consistently unconstitutional (foreclosure sign), or do they leave room for living-constitutionalist reasoning in other doctrinal areas (coexistence sign)?',
    'If originalism forecloses living constitutionalism, the cs_structure.reading_relations should record forecloses. If they coexist as institutional competitors, it should record coexists_with. This determines whether the constraint''s legitimacy claim (that originalism is THE correct method) is a logical truth or an institutional assertion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalism_foreclosure_vs_coexistence, conceptual, 'Whether originalism and living constitutionalism logically foreclose each other or coexist as competing readings.').

omega_variable(
    extraction_or_coordination_primacy,
    'Is the measured extraction (0.62) the primary function of the constraint, or is it a side effect of the genuine coordination function (stable interpretation)? Can the coordination function be preserved while reducing extraction through alternative institutional designs (e.g., originalism applied more flexibly, or historical-evidence constraints without originalist rigor)?',
    'Institutional comparison: examine jurisdictions or constitutional systems that employ historical-evidence constraints without the originalist framework''s rigidity (e.g., some comparative constitutional courts use original intent as one interpretive tool among several). Measure: do they achieve interpretive stability comparable to originalist-dominated systems, and do they show lower extraction costs for rights-claimants and social movements? If stable interpretation is achievable with lower extraction, the extraction is contingent on originalism''s particular institutional design, not inherent to the coordination function.',
    'If extraction is contingent, the constraint might be redesignatable as rope (genuine coordination, minimal extraction) with institutional reform. If extraction is inherent to historical-evidence gating, it remains tangled_rope or approaches snare. This affects mandatrophy analysis: is the constraint justified by its coordination function, or does the extraction exceed what coordination requires?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_or_coordination_primacy, empirical, 'Whether extraction is essential to the coordination function or contingent on originalism''s institutional design.').

omega_variable(
    dead_hand_objection_to_originalism,
    'Does the constraint''s core axiom — that meaning is fixed at ratification — constitute an impermissible transfer of authority from the living present to the dead Framers? Is this axiom itself a normative claim about political legitimacy, masquerading as a constraint on interpretation?',
    'Philosophical analysis: compare originalism''s Dead Hand axiom against non-originalist claims that the Constitution should reflect living democratic will. Determine whether the choice to bind current generations to historical meaning is itself a normative political claim (in which case originalism is not neutral constraint discovery but partisan choice of whose values govern), or whether it is a logically prior epistemic claim (historical evidence IS the relevant evidence for meaning, independent of political values).',
    'If the Dead Hand axiom is a normative political claim masquerading as constraint, originalism is itself a reading (one interpretation of what legitimacy requires) rather than a neutral method. This would shift originalism from ''constraint on interpretation'' to ''one contested position in ongoing political negotiation about what the Constitution means.'' The classification might shift from tangled_rope (coordinating interpretation while extracting from certain groups) to snare (extraction disguised as neutral constraint discovery).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dead_hand_objection_to_originalism, preference, 'Whether originalism''s core axiom about binding to historical meaning is a normative political claim or a neutral epistemic constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text_authority__originalist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text_authority__originalist_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(cons_tr_t0, observed).
narrative_ontology:measurement(cons_tr_t8, constitutional_text_authority__originalist_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement_basis(cons_tr_t8, observed).
narrative_ontology:measurement(cons_tr_t16, constitutional_text_authority__originalist_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement_basis(cons_tr_t16, observed).
narrative_ontology:measurement(cons_tr_t25, constitutional_text_authority__originalist_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(cons_tr_t25, observed).
narrative_ontology:measurement(cons_tr_t35, constitutional_text_authority__originalist_reading, theater_ratio, 35, 0.41).
narrative_ontology:measurement_basis(cons_tr_t35, projected).
narrative_ontology:measurement(cons_tr_t50, constitutional_text_authority__originalist_reading, theater_ratio, 50, 0.41).
narrative_ontology:measurement_basis(cons_tr_t50, projected).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text_authority__originalist_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(cons_be_t0, observed).
narrative_ontology:measurement(cons_be_t8, constitutional_text_authority__originalist_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement_basis(cons_be_t8, observed).
narrative_ontology:measurement(cons_be_t16, constitutional_text_authority__originalist_reading, base_extractiveness, 16, 0.56).
narrative_ontology:measurement_basis(cons_be_t16, observed).
narrative_ontology:measurement(cons_be_t25, constitutional_text_authority__originalist_reading, base_extractiveness, 25, 0.6).
narrative_ontology:measurement_basis(cons_be_t25, observed).
narrative_ontology:measurement(cons_be_t35, constitutional_text_authority__originalist_reading, base_extractiveness, 35, 0.62).
narrative_ontology:measurement_basis(cons_be_t35, projected).
narrative_ontology:measurement(cons_be_t50, constitutional_text_authority__originalist_reading, base_extractiveness, 50, 0.62).
narrative_ontology:measurement_basis(cons_be_t50, projected).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text_authority__originalist_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(cons_su_t0, observed).
narrative_ontology:measurement(cons_su_t8, constitutional_text_authority__originalist_reading, suppression_requirement, 8, 0.51).
narrative_ontology:measurement_basis(cons_su_t8, observed).
narrative_ontology:measurement(cons_su_t16, constitutional_text_authority__originalist_reading, suppression_requirement, 16, 0.54).
narrative_ontology:measurement_basis(cons_su_t16, observed).
narrative_ontology:measurement(cons_su_t25, constitutional_text_authority__originalist_reading, suppression_requirement, 25, 0.57).
narrative_ontology:measurement_basis(cons_su_t25, observed).
narrative_ontology:measurement(cons_su_t35, constitutional_text_authority__originalist_reading, suppression_requirement, 35, 0.58).
narrative_ontology:measurement_basis(cons_su_t35, projected).
narrative_ontology:measurement(cons_su_t50, constitutional_text_authority__originalist_reading, suppression_requirement, 50, 0.58).
narrative_ontology:measurement_basis(cons_su_t50, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text_authority__originalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_text_authority__originalist_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_text_authority__originalist_reading, constitutional_text_authority__living_constitutionalist_reading).
narrative_ontology:affects_constraint(constitutional_text_authority__originalist_reading, constitutional_text_authority__positivist_reading).
narrative_ontology:affects_constraint(constitutional_text_authority__originalist_reading, unenumerated_rights_recognition_constraint).
narrative_ontology:affects_constraint(constitutional_text_authority__originalist_reading, article_v_amendment_difficulty).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel constitutional_text_authority. The originalist reading asserts meaning is fixed at ratification and derives from historical public understanding. Sibling readings — living constitutionalism and legal positivism — instantiate different constraints with different ε values and beneficiary/victim structures. The three readings compete for institutional dominance but coexist across different judicial seats and scholarly communities. Each reading is a separate story; this story captures ONLY the originalist instantiation. Links to siblings via network.affects_constraints indicate structural influence: originalism's enforcement affects the viability of living-constitutionalist doctrine (living-constitutionalist justices must argue against historical-evidence requirements), and positivism's adoption would eliminate the moral-content requirement that originalism uses to gate interpretation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_text_authority__originalist_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
