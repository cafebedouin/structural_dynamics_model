% ============================================================================
% CONSTRAINT STORY: us_constitution_interpretive__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_interpretive__originalist_reading, []).

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
 *   constraint_id: us_constitution_interpretive__originalist_reading
 *   human_readable: US Constitution: Originalist Interpretive Authority
 *   domain: constitutional_law/legal_interpretation/political_theory
 *
 * SUMMARY:
 *   Originalism is one reading of the contested US Constitution kernel. This
 *   reading asserts that constitutional meaning is fixed at ratification
 *   (1787 or the date of ratification of each amendment) and that judges must
 *   interpret according to the Framers' intent or the text's original public
 *   meaning. This reading constrains federal power, elevates state authority,
 *   protects property rights, and blocks unenumerated-rights claims. The
 *   constraint's operation imposes costs on those seeking federal regulatory
 *   expansion and contemporary civil rights protection while benefiting
 *   federalism advocates, property rights defenders, and constituencies whose
 *   preferred liberties align with historical understanding. The extraction
 *   metrics reflect that originalism is enforced—it requires active judicial
 *   adherence and opposition to competing methodologies—and that it
 *   asymmetrically benefits some constituencies while imposing costs on
 *   others. Theater ratio suggests that originalist reasoning sometimes
 *   performs constraint-maintenance rhetorics ('fidelity to text,' 'rule of
 *   law,' 'judicial restraint') when the underlying operation is choosing
 *   which historical sources to credit and which to marginalize. The
 *   measurement series captures the rise of originalism from a marginal
 *   position in constitutional theory (1980s) to near-dominance of the
 *   Supreme Court (2020s), with a slight retreat in theater ratio by 2026
 *   (reflecting increased academic and public scrutiny of originalist
 *   historical claims).
 *
 * KEY AGENTS:
 *   - originalist_judicial_coalition: Institutional power, agenda-setter, enforces originalist methodology through written opinions binding lower courts.
 *   - conservative_legal_movement: Organized power, beneficiary, funds originalist litigation and intellectual infrastructure.
 *   - federalism_advocates: Organized power, beneficiary, win when originalism constrains federal authority.
 *   - religious_liberty_constituency: Moderate power, identity-locked beneficiary, fused to religious identity, benefits from originalist Free Exercise interpretation.
 *   - property_rights_defenders: Powerful, arbitrage exit, beneficiary, benefit from originalist property doctrine and arbitrage across jurisdictions.
 *   - unenumerated_rights_claimants: Powerless, trapped, payer, bear costs when privacy/dignity/bodily autonomy claims fail under originalism.
 *   - federal_regulatory_expansion_advocates: Organized, constrained exit, payer, lose regulatory authority when originalism narrows Commerce/Necessary and Proper clauses.
 *   - contemporary_civil_rights_movements: Moderate power, identity-locked, payer, trapped to their cause, pay in narrowed legal remedies.
 *   - amendment_politics_constituency: Powerless, trapped, excluded, structurally barred from amending the Constitution without supermajority consensus (the only remedy when originalism forecloses their claims).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_interpretive__originalist_reading, 0.58).
domain_priors:suppression_score(us_constitution_interpretive__originalist_reading, 0.42).
domain_priors:theater_ratio(us_constitution_interpretive__originalist_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, accessibility_collapse, 0.67).
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_interpretive__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_interpretive__originalist_reading, "US Constitution: Originalist Interpretive Authority").
narrative_ontology:topic_domain(us_constitution_interpretive__originalist_reading, "constitutional_law/legal_interpretation/political_theory").

domain_priors:requires_active_enforcement(us_constitution_interpretive__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_interpretive__originalist_reading, '0ffb3eb8-e35a-4b50-a23f-ee718a1954c0').
narrative_ontology:cs_kernel_codification('0ffb3eb8-e35a-4b50-a23f-ee718a1954c0', fixed_text).
narrative_ontology:cs_authority_grounding('0ffb3eb8-e35a-4b50-a23f-ee718a1954c0', lineage).
narrative_ontology:cs_interpretation_layer_present('0ffb3eb8-e35a-4b50-a23f-ee718a1954c0').
narrative_ontology:cs_reading_relation('0ffb3eb8-e35a-4b50-a23f-ee718a1954c0', us_constitution_interpretive__living_constitution_reading, coexists_with).
narrative_ontology:cs_reading_relation('0ffb3eb8-e35a-4b50-a23f-ee718a1954c0', us_constitution_interpretive__popular_constitutionalism_reading, coexists_with).
narrative_ontology:cs_axiom('0ffb3eb8-e35a-4b50-a23f-ee718a1954c0', foundational, constitutional_meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(constitutional_meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('0ffb3eb8-e35a-4b50-a23f-ee718a1954c0', constitutional_meaning_fixed_at_ratification, deontological).
narrative_ontology:cs_axiom('0ffb3eb8-e35a-4b50-a23f-ee718a1954c0', foundational, judicial_fidelity_constrains_discretion).
narrative_ontology:cs_axiom_status(judicial_fidelity_constrains_discretion, holdable).
narrative_ontology:cs_axiom_grounding('0ffb3eb8-e35a-4b50-a23f-ee718a1954c0', judicial_fidelity_constrains_discretion, empirically_contingent).
narrative_ontology:cs_axiom('0ffb3eb8-e35a-4b50-a23f-ee718a1954c0', secondary, historical_intent_is_discoverable_and_determinative).
narrative_ontology:cs_axiom_status(historical_intent_is_discoverable_and_determinative, holdable).
narrative_ontology:cs_axiom_grounding('0ffb3eb8-e35a-4b50-a23f-ee718a1954c0', historical_intent_is_discoverable_and_determinative, empirically_contingent).
narrative_ontology:cs_reference_frame('0ffb3eb8-e35a-4b50-a23f-ee718a1954c0', framers_intent_fixed_meaning).
narrative_ontology:cs_drift_state('0ffb3eb8-e35a-4b50-a23f-ee718a1954c0', contemporary_originalist_ascendancy_2024, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0ffb3eb8-e35a-4b50-a23f-ee718a1954c0', '').
narrative_ontology:cs_kernel_id(us_constitution_interpretive__originalist_reading, us_constitution_interpretive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, federalism_advocates).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, original_understanding_religious_liberty_claimants).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, property_rights_defenders).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, limited_government_constituency).
narrative_ontology:constraint_victim(us_constitution_interpretive__originalist_reading, unenumerated_rights_claimants).
narrative_ontology:constraint_victim(us_constitution_interpretive__originalist_reading, federal_regulatory_expansion_advocates).
narrative_ontology:constraint_victim(us_constitution_interpretive__originalist_reading, contemporary_civil_rights_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, conservative_legal_movement).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, religious_liberty_constituency).
narrative_ontology:constraint_victim(us_constitution_interpretive__originalist_reading, living_constitutionalist_judges).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Justices and lower court judges who adopt originalist methodology. They interpret the Constitution by researching the text's original public meaning at ratification or the Framers' intent, applying that fixed meaning to contemporary cases. They argue this constrains judicial discretion and honors the rule of law. They enforce originalism through written opinions that bind lower courts and shape legal doctrine.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, originalist_judicial_coalition, agenda_setter,
    institutional, generational, analytical, national).

% Network of originalist scholars, think tanks, bar associations, and litigants organized around originalist constitutional theory. They benefit from originalist rulings that narrow federal power, protect property rights, and restore state authority. They fund legal challenges designed to shift jurisprudence originalist-ward, train judges in originalist methodology, and build intellectual legitimacy for the reading.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, conservative_legal_movement, beneficiary,
    organized, generational, constrained, national).

% Include state governments, limited-government advocates, and Tenth Amendment activists. Originalist rulings that constrain federal Commerce Clause and Spending Clause power return authority to states. They benefit from decisions invalidating federal mandates (e.g., Medicaid expansion conditions, environmental regulations read beyond enumerated powers) and win when originalism narrows the federal scope.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, federalism_advocates, beneficiary,
    organized, generational, constrained, national).

% Religious organizations and believers who view originalism as protective of their liberty. Originalist interpretation of the Free Exercise Clause has shifted to prioritize religious accommodation over neutral laws of general applicability (e.g., Employment Division v. Smith overruled by subsequent originalist decisions). They are locked into the religious-identity frame and view originalist protection as non-negotiable.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, religious_liberty_constituency, beneficiary,
    moderate, biographical, identity_locked, national).

% Corporate and individual property owners, business associations, and libertarian advocates. They benefit from originalist constraints on regulatory takings doctrine, environmental law, and labor regulation. Their exit option is arbitrage: they can structure holdings and operations across multiple jurisdictions or seek refuge in friendly regulatory environments. They actively litigate to advance originalist property theories.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, property_rights_defenders, beneficiary,
    powerful, biographical, arbitrage, national).

% Individuals and movements claiming rights not enumerated in the Constitution (privacy, dignity, bodily autonomy, intimate association). Under originalism, these claims fail unless the Framers intended to protect them. They bear the cost when originalist rulings reject unenumerated-rights theories (e.g., access to abortion, same-sex marriage, gender identity protection). They are trapped because they cannot exit the jurisdiction and have no recourse except constitutional amendment.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, unenumerated_rights_claimants, payer,
    powerless, biographical, trapped, national).

% Progressives, environmental advocates, labor advocates, and civil rights coalitions who rely on federal regulatory authority to address market failures, interstate externalities, and national disparities. Originalist constraints on the Commerce Clause, Necessary and Proper Clause, and delegation doctrine narrow the federal regulatory toolkit they depend on. They pay through reduced regulatory capacity and lose cases where originalism defeats federal statutes.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, federal_regulatory_expansion_advocates, payer,
    organized, generational, constrained, national).

% Justices and lower court judges who adopt competing interpretive methodologies (purposivism, evolving standards, reasonable adaptation). They bear a structural cost: originalist dominance on the Supreme Court constrains their authority to develop doctrine through their own methodologies. Their status as 'observer' reflects their analytical capacity to describe the originating constraint; their 'payer' role reflects their de facto subordination in the judicial hierarchy.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, living_constitutionalist_judges, payer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_interpretive__originalist_reading, living_constitutionalist_judges, observer).

% Movements for racial justice, LGBTQ+ rights, gender equality, and disability access. They are identity-locked to their cause and cannot exit. Under originalism, many contemporary civil rights claims must trace to Framers' intent (a nearly impossible historical archaeology), whereas competing methodologies permit reasoning from contemporary understanding of equal dignity. They pay through narrowed legal remedies and must engage in costly constitutional-amendment campaigns to expand protection.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, contemporary_civil_rights_movements, payer,
    moderate, biographical, identity_locked, national).

% Academic historians and constitutional scholars who study the Framers' intent and the original public meaning. They provide the evidential base for originalist legal arguments but remain external to the legal system. They are observers who document what 'original meaning' was, aware that their scholarship is read by judges and shaped by the stakes courts attach to historical accuracy.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, historical_scholarship_community, observer,
    analytical, generational, analytical, national).

% Citizens and movements that might pursue constitutional amendment to overcome originalist constraints (e.g., to enshrine privacy rights, expand federal authority, or lock in contemporary civil rights). They are structurally excluded because amendment requires super-majoritarian consensus (2/3 of both houses plus 3/4 of states), making their voice heard only after decades of political organizing. The originalist constraint makes amendment the only remedy, but amendment is deliberately hard.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, amendment_politics_constituency, excluded,
    powerless, civilizational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_interpretive__originalist_reading, originalist_judicial_coalition).
narrative_ontology:fixing_cost_class(us_constitution_interpretive__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Originalist methodology solves the problem of constraining judicial discretion by tethering interpretation to a fixed historical reference point. Rather than each judge applying their own contemporary values, originalism coordinates on a common interpretive reference (Framers' intent or original public meaning), reducing variance in how the Constitution is read and creating predictable boundaries for federal power and individual liberty.
% TRANSFER_FUNCTION: Shifts the legitimacy of certain constitutional claims from contemporary democratic consensus or evolving standards back to historical authorial intent. This movement transfers political authority from living legislatures and contemporary movements to the dead Framers and historical texts—those whose intent controls lose power to amend doctrine without formal amendment. Concretely: federal power is narrowed, state power is enlarged, property rights receive heightened protection, unenumerated rights claims are rejected unless historically grounded.
% ABSENT_VOICES: Constitutional scholars outside the originalist tradition (purposivists, living-constitutionalists, critical scholars); contemporary civil rights movements not grounded in historical precedent; future generations who will live under the boundaries originalism fixes; citizens who believe the Constitution should evolve but lack a voice in judicial interpretation (excluded through the amendment supermajority gate).
% DISAPPEARANCE_RATIONALE: If originalist interpretive authority disappeared overnight and judges reverted to competing methodologies (living constitutionalism, purposivism, evolving standards), vast swaths of federal regulatory authority would be restored, unenumerated-rights doctrine would expand, and state power would contract. This would permit federal climate regulation, voting-rights protection, and labor standards the originalist reading constrains. The institutional arrangement would reorganize radically around the judge's permitted reasoning methods.
% FOUNDING_PROBLEM: The constitutional text requires interpretation because changed circumstances and ambiguous language make meaning contestable. Without a shared interpretive method, judicial power appears unbounded—each judge's personal values would substitute for law, creating rule-of-law failure and political chaos. Originalism was revived (the constraint originates in constitutional theory circa 1980s–1990s) to anchor interpretation to a fixed external standard: what the Framers understood, not what judges prefer.
% FOUNDING_PROBLEM_CORROBORATION: Originalist judges and scholars attest the founding problem is live: judicial subjectivity remains a risk and originalism mitigates it. Living constitutionalists and progressive legal scholars attest the founding problem is misidentified—that originalism's constraint on interpretation doesn't prevent subjectivity but rather shifts discretion to the historical question 'what did they mean?', which is equally contestable. Independent constitutional law scholars document that historical research produces contested, not unanimous, conclusions about original meaning; the 'fixed anchor' claim is itself contested and not external to interpretation.
narrative_ontology:disappearance_verdict(us_constitution_interpretive__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_interpretive__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_interpretive__originalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_interpretive__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_interpretive__originalist_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_interpretive__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_interpretive__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_interpretive__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 1980 to 2022 (0.28 → 0.61) as originalism accumulates judicial power and the costs of its constraints become visible: federal regulatory reach shrinks, unenumerated rights fail, property rights expand. The slight decline by 2026 (0.61 → 0.58) reflects growing academic and public backlash against originalist historical claims, erosion of its 'neutral constraint' framing, and strategic countermeasures (living constitutionalist writing, amendment campaigns). Suppression stays moderate (0.42) because originalist methodology is authored as constraint-by-reasoning, not crude coercion—judges police each other's originalist fidelity through footnotes and doctrinal criticism, not (explicitly) through punitive exclusion. However, suppression of living-constitutionalist reasoning from the Supreme Court majority is real (lower courts must follow precedent; dissenting judges are unheard in the majority opinion). Theater ratio rises from 0.12 to 0.35 over the interval, then declines slightly to 0.31—originalism's early framing as neutral 'textualism' performs well on theater metrics; as the empirical results (property-rights wins, unenumerated-rights losses, federal regulatory defeats) become visible, the performative value of 'fidelity to text' and 'judicial restraint' degrades. The highest theater ratio (2022) coincides with the Dobbs decision overruling Roe v. Wade—a major originalist victory that made the policy-choosing dimensions of originalism visible (if it were truly neutral constraint, why did it suddenly reverse 50 years of precedent?). Accessibility collapse is high (0.67) because once the originalist interpretive frame is adopted, alternatives (asking 'what would serve contemporary values?', 'what would contemporary equal protection permit?') become literally foreclosed as illegitimate—originalism collapses the semantic space in which competing methodologies can be heard. Resistance is high (0.73) because living constitutionalists, progressive legal movements, civil rights advocates, and those seeking federal regulatory authority mount sustained intellectual and political resistance to originalism. They argue for alternative methodologies, file briefs in originalist cases, engage in amendment campaigns, and organize against originalist judicial nominees.
 *
 * PERSPECTIVAL GAP:
 *   The originalist judicial coalition and conservative legal movement experience originalism as principled constraint that limits judicial discretion and honors the rule of law—they see the coordination function and believe suppression is minimal (just enforcing the methodology). Unenumerated-rights claimants, civil rights movements, and federal regulatory advocates experience originalism as coercive power that closes interpretive options and forecloses their claims—they see the extraction and feel the suppression of competing methodologies. Federalism advocates and property-rights defenders experience originalism as liberation (expanding their authority or protection) and see no extraction, only correction of prior judicial excess. The engine computes these divergences from directionality: agenda-setter + beneficiary seats get low d (beneficiary end); payer + trapped + identity-locked seats get high d (target end). The 'perspectival gap' IS the structural divergence in their relation to the constraint: the mechanism operates differently for each seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Originalist judicial coalition (institutional power, agenda-setter): d ≈ 0.1 (near beneficiary end). Sets the rules, enforces the methodology, operates within it without bearing its costs. Exit options are analytical (theoretical debate, not practical exit). Beneficiary organizations (conservative legal movement, federalism advocates): d ≈ 0.15–0.25. They organize around originalism, it serves their interests, and they have constrained exit (they could defect to living constitutionalism but are organizationally committed). Property-rights defenders (powerful, arbitrage): d ≈ 0.2. They benefit from originalist property doctrine and have arbitrage exit (can relocate assets/operations to friendly jurisdictions, reducing local regulatory burden). Religious liberty constituency (moderate, identity-locked): d ≈ 0.35. Benefits from originalist Free Exercise protection but is identity-locked to religious identity—cannot exit the religious frame itself. Exit options are severely constrained by self-definition. Unenumerated-rights claimants (powerless, trapped): d ≈ 0.85–0.95 (near full-target end). Bear the extraction of constrained interpretive options, cannot exit the jurisdiction, identity-locked to their dignity/privacy/bodily-autonomy claims. Federal regulatory advocates (organized, constrained): d ≈ 0.75–0.85. Lose regulatory authority when originalism constrains Commerce Clause, constrained to the regulatory-expansion frame by their organization's mission. Contemporary civil rights movements (moderate, identity-locked): d ≈ 0.80. Identity-locked to their movement, cannot exit, bear costs when civil rights claims fail under originalism. Amendment-politics constituency (powerless, trapped, excluded): d ≈ 0.88. The highest: structurally excluded from the interpretive process, must pursue costly constitutional amendment as sole remedy, no practical exit. Directionality overrides: None needed—the structural derivation from beneficiary/victim + exit + power produces accurate directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   Originalism's founding problem (constraining judicial discretion via fixed historical reference) is LIVE but increasingly contested. The constraint is NOT experiencing mandatrophy sensu stricto (function atrophied while structure persists through inertia). Instead, originalism's mandate—constraining interpretation—IS being executed, but the empirical claim that originalism actually constrains judges (rather than merely selecting which discretionary moves are cosmetically justified) is increasingly disputed. Originalism was presented as a neutral methodology that removes judicial choice; accumulated judicial practice shows originalism permits substantial discretion in selecting which historical sources to credit, which levels of generality to adopt when describing historical meaning, and how to resolve contradictions between historical sources. The theater ratio's rise to 0.35 reflects this: the constraint persists by performing fidelity when the underlying operation selects which history to believe. This is NOT yet mandatrophy (function-collapse), but it is 'the mandate is contested.' The engine will flag this: if the constraint's claimed purpose (constraining discretion) is empirically false while the constraint persists (because powerful agents benefit), the classification could shift from tangled_rope toward piton (atrophied function maintained through theater). The current classification (tangled_rope: genuine coordination function + asymmetric extraction) holds because originalism DOES coordinate on a common methodological framework—it does reduce variance relative to pure discretion. But the omega variables flag the empirical contestability of the founding problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_meaning_discoverability,
    'Is the Framers'' intent or the original public meaning empirically discoverable and uncontestable, or does historical reconstruction itself involve interpretive choices that undermine originalism''s claim to external constraint?',
    'Comparative historical analysis of originalist judges'' and scholars'' disagreements about original meaning on the same constitutional provisions. If experts disagree substantially about what the original meaning was, the constraint''s grounding in ''fixed, discoverable meaning'' is falsified.',
    'If original meaning is not uniquely discoverable, originalism''s core legitimating claim—that it constrains judicial discretion via reference to an external, fixed standard—fails. The constraint becomes less tangled_rope (genuine coordination on a methodology) and more piton (the apparent methodology is performative; judges select which history to credit, and fidelity language masks discretion).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(original_meaning_discoverability, empirical, 'Whether originalism''s putative external constraint (historical meaning) is actually external or whether it requires interpretive choices within history itself.').

omega_variable(
    competing_reading_logical_exclusion,
    'Does the originalist reading logically foreclose living constitutionalism and popular constitutionalism, or can different parties coherently hold each reading within their own frameworks?',
    'Doctrinal analysis: Do the foundational premises of originalism (meaning is fixed at ratification) and living constitutionalism (meaning evolves) logically contradict such that no single judge or framework could endorse both? Or do they merely reflect different institutional choices (originalist judges apply fixed meaning, living-constitutionalist judges apply evolving meaning) that coexist across a divided judiciary?',
    'If the readings logically foreclose one another, the relationship is forecloses; if they coexist across different judicial seats and political movements without internal contradiction within each seat''s framework, the relationship is coexists_with. Classification determines whether originalism is a genuine alternative methodology or a foundational paradigm shift that displaces its competitors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competing_reading_logical_exclusion, conceptual, 'Whether originalism and its sibling readings are logically incommensurable or coexisting alternatives.').

omega_variable(
    supermajority_amendment_gate_suppression,
    'Is the supermajority threshold for constitutional amendment (2/3 both houses, 3/4 states) a structural feature of the US system independent of originalism, or does originalism''s constraint that meaning is FIXED intensify the suppression of amendment by making it the sole remedy for interpretive disagreement?',
    'Comparative analysis: In periods when living constitutionalism dominated (ca. 1960s–1980s), did the amendment supermajority feel equally suppressive? Or does originalism''s rigidity (can''t evolve meaning via interpretation) make amendment the ONLY option and thus intensify its suppression? If originalism makes amendment salient and necessary, it has intensified suppression via framing.',
    'If originalism intensifies amendment suppression, the measured suppression (0.42) understates the structural coercion: the constraint forces amendment (near-impossible) as the sole remedy, which is a form of suppression not captured by direct judicial coercion. The suppression metric would need revision upward, and the classification could shift from tangled_rope toward snare (suppression + extraction without genuine coordination benefit).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(supermajority_amendment_gate_suppression, conceptual, 'Whether originalism''s rigidity amplifies the suppressive effect of the constitutional amendment threshold.').

omega_variable(
    sibling_reading_extraction_asymmetry,
    'If living constitutionalism or popular constitutionalism were ascendant (rather than originalism), would the extraction metrics and beneficiary/victim structure be inverted, or would one reading reduce extraction while another increases it?',
    'Author constraint stories for the living_constitution_reading and popular_constitutionalism_reading with their own extraction metrics and beneficiary/victim structures. Compare the three readings'' extraction profiles to determine whether one reading genuinely reduces system-wide extraction or merely redistributes it.',
    'If all three readings produce comparable extraction (just different beneficiary/victim sets), then no reading is objectively better; each constrains one group while liberating another. If one reading demonstrably reduces extraction (lower extractiveness, broader beneficiary base, fewer victims), that reading''s ascendance would reduce system-level extraction. This determines whether orignal reading choice has constitutional significance beyond power redistribution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_extraction_asymmetry, empirical, 'Whether sibling readings have asymmetric extraction profiles or merely redistribute extraction across constituencies.').

omega_variable(
    identity_locked_exit_modulation,
    'For religious liberty claimants, is their identity-lock to originalism a result of genuine alignment between religious identity and originalist doctrine, or does it reflect institutional capture of religious identity-narratives by originalist legal movements?',
    'Empirical: survey contemporary religious leaders and scholars on whether originalism is theologically necessary or merely strategically aligned with current originalist-movement power. Historical: examine whether religious-liberty protection was equally strong under living constitutionalism (e.g., RFRA era, 1993–present). If religious liberty flourished under non-originalist methodologies, the current identity-lock may be constructed alignment rather than intrinsic.',
    'If identity-lock is constructed, the directionality for religious liberty claimants (d ≈ 0.35) understates their actual extraction: they believe they are beneficiaries of an ideologically aligned reading, but they are actually captured within an asymmetric constraint. Reclassification would increase their d toward 0.5–0.65 (symmetric-to-target), and the overall constraint could shift from tangled_rope toward snare (extraction disguised as ideological alignment).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_exit_modulation, empirical, 'Whether religious-liberty claimants'' perceived alignment with originalism reflects genuine ideological coherence or institutional capture of religious narratives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_interpretive__originalist_reading, 1980, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1980, us_constitution_interpretive__originalist_reading, theater_ratio, 1980, 0.12).
narrative_ontology:measurement(us_c_tr_t1995, us_constitution_interpretive__originalist_reading, theater_ratio, 1995, 0.18).
narrative_ontology:measurement(us_c_tr_t2005, us_constitution_interpretive__originalist_reading, theater_ratio, 2005, 0.24).
narrative_ontology:measurement(us_c_tr_t2015, us_constitution_interpretive__originalist_reading, theater_ratio, 2015, 0.28).
narrative_ontology:measurement(us_c_tr_t2022, us_constitution_interpretive__originalist_reading, theater_ratio, 2022, 0.35).
narrative_ontology:measurement(us_c_tr_t2026, us_constitution_interpretive__originalist_reading, theater_ratio, 2026, 0.31).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1980, us_constitution_interpretive__originalist_reading, base_extractiveness, 1980, 0.28).
narrative_ontology:measurement(us_c_be_t1995, us_constitution_interpretive__originalist_reading, base_extractiveness, 1995, 0.39).
narrative_ontology:measurement(us_c_be_t2005, us_constitution_interpretive__originalist_reading, base_extractiveness, 2005, 0.48).
narrative_ontology:measurement(us_c_be_t2015, us_constitution_interpretive__originalist_reading, base_extractiveness, 2015, 0.54).
narrative_ontology:measurement(us_c_be_t2022, us_constitution_interpretive__originalist_reading, base_extractiveness, 2022, 0.61).
narrative_ontology:measurement(us_c_be_t2026, us_constitution_interpretive__originalist_reading, base_extractiveness, 2026, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1980, us_constitution_interpretive__originalist_reading, suppression_requirement, 1980, 0.25).
narrative_ontology:measurement(us_c_su_t1995, us_constitution_interpretive__originalist_reading, suppression_requirement, 1995, 0.32).
narrative_ontology:measurement(us_c_su_t2005, us_constitution_interpretive__originalist_reading, suppression_requirement, 2005, 0.38).
narrative_ontology:measurement(us_c_su_t2015, us_constitution_interpretive__originalist_reading, suppression_requirement, 2015, 0.41).
narrative_ontology:measurement(us_c_su_t2022, us_constitution_interpretive__originalist_reading, suppression_requirement, 2022, 0.44).
narrative_ontology:measurement(us_c_su_t2026, us_constitution_interpretive__originalist_reading, suppression_requirement, 2026, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_interpretive__originalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_interpretive__originalist_reading, 0.12).
narrative_ontology:affects_constraint(us_constitution_interpretive__originalist_reading, us_constitution_interpretive__living_constitution_reading).
narrative_ontology:affects_constraint(us_constitution_interpretive__originalist_reading, us_constitution_interpretive__popular_constitutionalism_reading).
narrative_ontology:affects_constraint(us_constitution_interpretive__originalist_reading, federal_commerce_clause_power).
narrative_ontology:affects_constraint(us_constitution_interpretive__originalist_reading, unenumerated_rights_protection).
narrative_ontology:affects_constraint(us_constitution_interpretive__originalist_reading, state_sovereign_immunity).
narrative_ontology:affects_constraint(us_constitution_interpretive__originalist_reading, property_takings_doctrine).

% DUAL FORMULATION NOTE:
% The US Constitution kernel decomposes into at least three structurally distinct constraint stories: originalist_reading (this file), living_constitution_reading, and popular_constitutionalism_reading. Each instantiates a different ε, beneficiary/victim structure, and measured extraction. They are not the same constraint viewed from different angles—they are different constraints generated by different readings of the same contested authority structure (the Constitution). The ε-invariance principle requires separate stories because the beneficiary/victim structure, enforcement mechanism, and empirical impacts diverge substantially across readings. Sibling readings are linked via network.affects_constraints (bidirectional or unidirectional depending on whether one reading logically forecloses or merely influences the other). This file represents the originalist reading; sibling files will author the living-constitutionalist and popular-constitutionalist readings with their own structural data and metrics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_interpretive__originalist_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
