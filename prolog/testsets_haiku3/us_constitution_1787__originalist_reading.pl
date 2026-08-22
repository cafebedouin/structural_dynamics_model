% ============================================================================
% CONSTRAINT STORY: us_constitution_1787__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_1787__originalist_reading, []).

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
 *   constraint_id: us_constitution_1787__originalist_reading
 *   human_readable: Constitutional Originalism: Meaning Fixed at Ratification (1787 Framers' Intent)
 *   domain: constitutional_law/legal_theory/political_philosophy
 *
 * SUMMARY:
 *   The originalist reading of the Constitution asserts that constitutional
 *   meaning was fixed at ratification (1787) and that the proper method of
 *   constitutional interpretation is to recover the framers' intent and the
 *   original public meaning of the text. This reading structures the
 *   interpretive boundary: claims outside the 1787 framers' conception
 *   (modern social rights, broad implied powers, unenumerated rights inferred
 *   from structure) are outside the Constitution's scope and require either
 *   new historical evidence linking to the founding moment or formal
 *   amendment. The constraint operates as a tangled rope: it genuinely
 *   coordinates on a single, stable interpretive method (benefiting from
 *   standardization), while simultaneously extracting from social movements
 *   whose rights claims fall outside the historical-evidence boundary. The
 *   high suppression reflects active enforcement through Supreme Court
 *   precedent and law school credentialing; the rising theater ratio over the
 *   interval reflects increasing effort devoted to defending the
 *   historical-evidence standard against critics who challenge its
 *   presuppositions.
 *
 * KEY AGENTS:
 *   - originalist_interpretive_community: controls academic and judicial validation of constitutional reasoning
 *   - property_rights_defenders: benefit from originalism's narrow reading of regulatory authority
 *   - federalism_advocates: benefit from originalism's skepticism toward expanded federal powers
 *   - social_rights_claimants: pay by having their rights claims foreclosed unless linked to 1787 evidence
 *   - progressive_constitutional_readers: pay by losing interpretive authority and institutional standing
 *   - supreme_court_majority: agenda-setter enforcing originalism through precedent
 *   - historical_evidence_gatekeepers: control the epistemic standard by which originalist claims are adjudicated
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_1787__originalist_reading, 0.68).
domain_priors:suppression_score(us_constitution_1787__originalist_reading, 0.71).
domain_priors:theater_ratio(us_constitution_1787__originalist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_1787__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_1787__originalist_reading, "Constitutional Originalism: Meaning Fixed at Ratification (1787 Framers' Intent)").
narrative_ontology:topic_domain(us_constitution_1787__originalist_reading, "constitutional_law/legal_theory/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_1787__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_1787__originalist_reading, 'ed3e9b57-f8f3-42ae-9544-d40e9346781a').
narrative_ontology:cs_kernel_codification('ed3e9b57-f8f3-42ae-9544-d40e9346781a', fixed_text).
narrative_ontology:cs_authority_grounding('ed3e9b57-f8f3-42ae-9544-d40e9346781a', lineage).
narrative_ontology:cs_interpretation_layer_present('ed3e9b57-f8f3-42ae-9544-d40e9346781a').
narrative_ontology:cs_reading_relation('ed3e9b57-f8f3-42ae-9544-d40e9346781a', us_constitution_1787__living_reading, forecloses).
narrative_ontology:cs_reading_relation('ed3e9b57-f8f3-42ae-9544-d40e9346781a', us_constitution_1787__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('ed3e9b57-f8f3-42ae-9544-d40e9346781a', foundational, meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('ed3e9b57-f8f3-42ae-9544-d40e9346781a', meaning_fixed_at_ratification, deontological).
narrative_ontology:cs_axiom('ed3e9b57-f8f3-42ae-9544-d40e9346781a', foundational, framers_intent_binding_authority).
narrative_ontology:cs_axiom_status(framers_intent_binding_authority, holdable).
narrative_ontology:cs_axiom_grounding('ed3e9b57-f8f3-42ae-9544-d40e9346781a', framers_intent_binding_authority, empirically_contingent).
narrative_ontology:cs_axiom('ed3e9b57-f8f3-42ae-9544-d40e9346781a', secondary, original_public_meaning_discoverable).
narrative_ontology:cs_axiom_status(original_public_meaning_discoverable, holdable).
narrative_ontology:cs_axiom_grounding('ed3e9b57-f8f3-42ae-9544-d40e9346781a', original_public_meaning_discoverable, empirically_contingent).
narrative_ontology:cs_reference_frame('ed3e9b57-f8f3-42ae-9544-d40e9346781a', fixed_constitutional_meaning_at_ratification).
narrative_ontology:cs_drift_state('ed3e9b57-f8f3-42ae-9544-d40e9346781a', contemporary_social_movements_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ed3e9b57-f8f3-42ae-9544-d40e9346781a', '').
narrative_ontology:cs_kernel_id(us_constitution_1787__originalist_reading, us_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, originalist_interpretive_community).
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, property_rights_defenders).
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, federalism_advocates).
narrative_ontology:constraint_victim(us_constitution_1787__originalist_reading, social_rights_claimants).
narrative_ontology:constraint_victim(us_constitution_1787__originalist_reading, modern_regulatory_defenders).
narrative_ontology:constraint_victim(us_constitution_1787__originalist_reading, progressive_constitutional_readers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Constitutional scholars, judges, and legal theorists who maintain that constitutional meaning is fixed at ratification and discoverable through historical research into framers' intent and original public meaning. They control the academic and judicial credentialing mechanisms that determine which interpretation frameworks count as legitimate legal reasoning. They benefit from the constraint because originalism's high evidentiary demands and narrow scope protect traditional interpretive authority against populist or activist rereadings.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, originalist_interpretive_community, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_1787__originalist_reading, originalist_interpretive_community, beneficiary).

% Property owners, business interests, and constitutional conservatives who benefit from originalism's narrow reading of regulatory authority and its skepticism toward modern expansive readings of commerce and police powers. The constraint legitimizes their resistance to contemporary regulatory takings claims and wealth-redistribution mandates.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, property_rights_defenders, beneficiary,
    powerful, generational, mobile, national).

% State governments and decentralization advocates who benefit from originalism's narrower reading of federal enumerated powers and its skepticism toward broad Commerce Clause and Section 5 readings that would expand federal reach into traditional state domains.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, federalism_advocates, beneficiary,
    powerful, generational, mobile, national).

% Advocates for expanded constitutional protection of social and economic rights (labor protections, healthcare access, anti-discrimination rights beyond enumerated categories, privacy rights not texturally rooted in 1787 understandings) who find their claims foreclosed by originalism's historical-evidence requirement and narrow reading of implied powers. They must produce historical evidence from 1787 or later constitutional moments to make claims; modern social necessity alone is insufficient under this constraint.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, social_rights_claimants, payer,
    organized, biographical, identity_locked, national).

% Administrative agencies, progressive legal scholars, and regulatory advocates whose policy domain rests on broad readings of federal enumerated powers (Commerce Clause, Necessary and Proper Clause, tax authority) and implied rights inferred from constitutional structure. Originalism's narrow reading of these powers constrains their authority-grounding and requires constant litigation to defend regulatory regimes against originalist takings and enumeration challenges.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, modern_regulatory_defenders, payer,
    institutional, generational, constrained, national).

% Constitutional scholars and advocacy lawyers committed to reading the Constitution as an evolving, aspirational document whose meaning responds to contemporary moral understanding and social movement. The originalist constraint forecloses their interpretive framework by law and judicial precedent; they must constantly argue for alternative canons (living constitutionalism, purposivism) while originalism holds institutional gatekeeping power over constitutional legitimacy.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, progressive_constitutional_readers, payer,
    organized, biographical, identity_locked, national).

% The current conservative-majority Supreme Court that enforces the originalist constraint through case law, precedent, and the doctrine of stare decisis. They set the standard for what counts as legitimate constitutional reasoning and police the boundaries of acceptable interpretation.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, supreme_court_majority, agenda_setter,
    institutional, generational, analytical, national).

% Legal education institutions and bar associations that determine what counts as competent constitutional reasoning and which interpretive frameworks are taught and validated as legitimate. The constraint operates through credentialing: lawyers trained primarily in originalist methods gain professional legitimacy; alternative methods are framed as less rigorous or politically motivated.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, law_schools_and_credentialing_bodies, agenda_setter,
    institutional, generational, analytical, national).

% Academic historians and historical scholars who control access to primary sources, interpretive frameworks, and credibility determinations about what 1787 framers intended. Under originalism, the constraint's epistemic demands give these scholars outsized authority in constitutional disputes: constitutional claims live or die based on historical evidence gatekeepers' assessments.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, historical_evidence_gatekeepers, agenda_setter,
    institutional, generational, analytical, national).

% Living constitutionalists, textualists using modern public meaning rather than original public meaning, purposivists, and other non-originalist interpretive communities that have been relegated to minority positions in appellate courts and law schools. They would argue that constitutional meaning must respond to evolving moral understanding and social conditions, but are structurally excluded from setting the interpretive standard.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, excluded_alternative_readings, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_1787__originalist_reading, originalist_interpretive_community).
narrative_ontology:fixing_cost_class(us_constitution_1787__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified, stable interpretive framework for constitutional meaning across courts and jurisdictions: if meaning were allowed to shift with contemporary values, the Constitution would become unpredictable and courts would face conflicting interpretive standards. Originalism coordinates on a single canonical method (historical evidence, original public meaning, enumerated powers principle).
% TRANSFER_FUNCTION: Transfers interpretive authority from contemporary social movements and democratic majorities to historical evidence specialists and conservative scholars whose expertise in 1787-era sources becomes the gatekeeping skill. It moves constitutional claims that depend on modern social necessity (social rights, regulatory authority, implied personal freedoms) from the realm of legitimate constitutional reasoning to the realm of what requires formal amendment or new historical evidence.
% ABSENT_VOICES: Living constitutionalists and progressive legal scholars occupy institutionally marginalized positions in current Supreme Court doctrine and mainstream legal education, though they remain present in law review literature and dissenting opinions. Excluded entirely are non-textual and non-historical interpretive frameworks (moral constitutionalism, feminist constitutional theory, critical race constitutional analysis) that are not seated at the deliberative table when constitutional meaning is adjudicated.
% DISAPPEARANCE_RATIONALE: Conservative constituencies and originalist scholars argue that if originalism disappeared and constitutional meaning became subject to living evolution, the Constitution would become a vague aspirational document whose meaning courts would redraw with political majorities, destroying the rule of law and enabling judicial overreach. Progressive constituencies argue that if originalism disappeared, constitutional law would return to what it was before the recent originalist ascendancy (1950s-1980s progressive constitutionalism, purposivism, flexible interpretation) — the world would not rearrange but would instead return to a prior stable state. The question is contested precisely because it turns on whether the prior state was functional or itself a degradation.
% FOUNDING_PROBLEM: The Constitution's text is fixed but its meaning is ambiguous. Early republic courts faced disputes about federal vs. state power, enumeration of constitutional rights, and what the text permitted. The framers' intent constraint was developed to answer: what does the Constitution actually permit, and who decides? The originalist answer: historical evidence of what the framers intended determines the boundary; courts enforce that boundary against contemporary reinterpretation.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars and conservative jurists attest the founding problem is still live: without originalism, courts make up constitutional meaning according to political preference. Progressive constitutional scholars and regulatory agencies attest the founding problem has shifted: the real problem is not ambiguous meaning but outdated meaning — the 1787 framers could not anticipate modern administrative states, telecommunications, or global commerce, and their specific intent is irrelevant to governing societies they could not imagine. Legal historians outside the originalist movement document that framers held diverse views and left gaps deliberately; historical evidence is fragmentary and admits multiple readings.
narrative_ontology:disappearance_verdict(us_constitution_1787__originalist_reading, contested).
narrative_ontology:founding_problem_status(us_constitution_1787__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_1787__originalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_1787__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_1787__originalist_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_1787__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_1787__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_1787__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts low (0.45) in the interval's early period because originalism was still primarily an academic position (1980s-2000s) without full institutional enforcement capacity. It rises steadily (reaching 0.68 by interval end) as the Supreme Court majority shifts toward originalism (2010s-2020s), consolidating the constraint through precedent and delegitimizing alternative frameworks. Suppression rises similarly: early originalist scholarship faced serious academic counter-argument; later institutional consolidation required active suppression of alternative methods (law school appointments, judicial credentials, precedent enforcement). Theater ratio is moderate but rising because originalism's scholarly apparatus is genuinely rigorous (historical research, textual analysis), but an increasing share of the enforcement work goes to defending the historical-evidence standard itself against epistemological criticism rather than doing constitutional interpretation. The grid and measurements run on one shared time scale, capturing the constraint's institutional strengthening over the interval.
 *
 * PERSPECTIVAL GAP:
 *   The originalist reading produces asymmetric extraction because historical evidence is path-dependent: rights and powers that were explicitly recognized in 1787 are protected; rights and powers that emerged later (privacy, sexual autonomy, gender equality, environmental protection, digital rights) must produce historical evidence of intent or remain outside the Constitution's scope. This asymmetry is structural to the reading, not a defect. Conservative beneficiaries have doctrine and history on their side; progressive claimants must reconstruct 18th-century intent for 21st-century problems.
 *
 * DIRECTIONALITY LOGIC:
 *   The originalist interpretive community sits near d=0.0 (full beneficiary): they control the interpretive standard, their career advancement depends on mastery of originalist methods, their institutional position is secured by the constraint's enforcement. Property-rights defenders and federalism advocates sit near d=0.2-0.4 (moderate beneficiaries): the constraint aligns with their policy interests but they do not control it directly. Social-rights claimants sit near d=0.85-1.0 (full targets): their claims are systematically foreclosed unless linked to 1787 evidence they must produce; their exit is identity-locked because the rights they claim are constitutive of modern identity frames (gender, sexuality, reproductive autonomy). Progressive constitutional readers sit near d=0.75-0.95 (targets): their entire methodological tradition is delegitimized; they are excluded from mainstream interpretation but cannot exit because constitutional law remains the domain of their professional identity.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled rope (rather than rope or snare) depends on whether genuine coordination exists beneath the extraction. The originalist defense is that constitutional stability requires a canonical method; without originalism courts become political. The counter-argument is that this coordination value is available from other methods (textualism, purposivism, living constitutionalism) with lower extraction costs. The empirical question becomes: does originalism uniquely provide stability, or does it provide stability + extraction? The measurement series shows extractiveness rising faster than stability-indicators would predict, suggesting extraction is accumulating faster than coordination is growing — a mandatrophy indicator. However, if we measure stability by Supreme Court consensus (justices agreeing on method), originalism has increased consensus at the cost of excluding alternative readings. That is tangled rope classic: genuine (narrow) coordination + asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    framers_intent_discoverability,
    'Is the framers'' intent actually recoverable and stable across different historical sources and interpretive methods, or do different valid historical inquiries produce conflicting intent readings?',
    'Comparative constitutional history: apply originalist methodology to the same constitutional clauses using different primary-source archives (Madison papers vs. state ratification conventions vs. contemporary newspapers). If the same clause yields multiple defensible intent readings, intent is under-determined.',
    'If intent is truly under-determined, the originalist constraint''s epistemic legitimacy collapses: it claims to anchor meaning to discoverable intent while intent is actually contested by historical evidence itself. The constraint would shift from tangled rope (genuine coordination + extraction) to snare (extraction using false coordination cover).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framers_intent_discoverability, empirical, 'Whether historical evidence supports a unique recovery of framers'' intent or admits multiple defensible readings.').

omega_variable(
    modern_social_necessity_vs_historical_requirement,
    'Should constitutional claims be foreclosed when they represent genuine moral and political consensus (majority of nations recognize these rights, modern expertise establishes necessity) but lack historical evidence from 1787?',
    'Case-law evolution: do courts develop exceptions to the historical-evidence requirement when modern necessity is sufficiently clear? Or does originalism maintain the requirement absolutely, effectively subordinating contemporary consensus to 18th-century framers'' imagination?',
    'If courts develop exceptions, originalism becomes a presumption rather than a constraint — it loses structural force. If the requirement holds absolutely, originalism''s extraction intensifies: modern consensus is treated as constitutionally illegitimate, regardless of social necessity or democratic support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modern_social_necessity_vs_historical_requirement, preference, 'Whether and how historical evidence requirements interact with contemporary social consensus in constitutional claims.').

omega_variable(
    coordinate_on_originalism_vs_coordinate_on_stability,
    'Is the genuine coordination function the use of originalism specifically, or is it the achievement of interpretive stability generally (which other methods could provide)?',
    'Counterfactual: if a living-constitutionalist majority Supreme Court emerged that also maintained consistent precedent and methodological rigor, would constitutional stability be preserved? If yes, the coordination value is stability, not originalism; originalism is just the current institutional choice with extraction layered on.',
    'If stability can be achieved by other methods, the tangled rope is a contingent institutional choice, not a structural necessity. The constraint could be replaced with lower-extraction coordination methods. If originalism uniquely provides stability (because only historical-evidence anchoring prevents arbitrary courts), then tangled rope is structurally necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordinate_on_originalism_vs_coordinate_on_stability, conceptual, 'Whether originalism is necessary for constitutional stability or one possible stabilization mechanism among others.').

omega_variable(
    epistemology_of_original_public_meaning,
    'Does ''original public meaning'' correspond to what the actual historical public understood, or does it correspond to what modern originalist scholars reconstruct as a theoretical public meaning?',
    'Epistemological analysis: compare historical-record evidence of what contemporaneous readers understood (state ratification debates, newspapers, private correspondence) with originalist scholars'' reconstructions. Do they align or diverge systematically?',
    'If they diverge, originalism is grounded in a counterfactual reconstruction (theoretical public meaning) rather than actual historical understanding. The constraint''s legitimacy shifts from historical-evidence grounding to methodology-chosen-by-scholars grounding — the extracted community would be paying for a scholar-constructed epistemic standard, not for fidelity to actual history.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epistemology_of_original_public_meaning, empirical, 'Whether original public meaning is historically observed or scholar-reconstructed.').

omega_variable(
    identity_fusion_in_progressive_constitutional_reading,
    'For progressive constitutional readers, how much of the identity-locked exit status reflects genuine structural/legal barriers versus internalized belief that constitutional law is the site of moral legitimacy?',
    'Counterfactual: if a progressive constitutional court majority emerged, would progressive scholars shift to operating in that framework, or do they experience constitutional law itself as identity-constitutive regardless of which reading holds power?',
    'If barrier is primarily structural (institutional gatekeeping), a shift in court majority would unlock exit and mobile mobility. If identity is internalized, progressive readers would remain in the constitutional-interpretation domain seeking vindication even if they had exit options — the suppression would persist post-barrier.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_fusion_in_progressive_constitutional_reading, empirical, 'Degree to which progressive constitutional scholars'' exit-locking is structural or identity-internalized.').

omega_variable(
    kernel_reading_alternative_formulations,
    'The originalist reading asserts meaning is fixed and intent-discoverable. Does this reading preclude the positivist reading (meaning fixed to text + amendments), or could a single framework hold both?',
    'Jurisprudential analysis: in cases where originalist and positivist interpretations diverge (e.g., unamended but expansively interpreted clauses like Commerce Clause), do they produce fundamentally incompatible constraint boundaries, or can a single justification framework accommodate both?',
    'If they are fundamentally incompatible, the relationship is `forecloses`. If they can be held within a single framework (e.g., ''meaning is fixed, but fixed to the text''s evolving application in democratic amendments''), the relationship is `coexists_with`. This determines whether the kernel exhibits tight logical structure or loose coalitional structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_alternative_formulations, conceptual, 'Whether originalism and positivism are logically incompatible or mutually compatible readings of constitutional authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_1787__originalist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_1787__originalist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(us_c_tr_t10, us_constitution_1787__originalist_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(us_c_tr_t20, us_constitution_1787__originalist_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(us_c_tr_t30, us_constitution_1787__originalist_reading, theater_ratio, 30, 0.39).
narrative_ontology:measurement(us_c_tr_t40, us_constitution_1787__originalist_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement(us_c_tr_t50, us_constitution_1787__originalist_reading, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_1787__originalist_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(us_c_be_t10, us_constitution_1787__originalist_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(us_c_be_t20, us_constitution_1787__originalist_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(us_c_be_t30, us_constitution_1787__originalist_reading, base_extractiveness, 30, 0.66).
narrative_ontology:measurement(us_c_be_t40, us_constitution_1787__originalist_reading, base_extractiveness, 40, 0.67).
narrative_ontology:measurement(us_c_be_t50, us_constitution_1787__originalist_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_1787__originalist_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(us_c_su_t10, us_constitution_1787__originalist_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(us_c_su_t20, us_constitution_1787__originalist_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(us_c_su_t30, us_constitution_1787__originalist_reading, suppression_requirement, 30, 0.69).
narrative_ontology:measurement(us_c_su_t40, us_constitution_1787__originalist_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(us_c_su_t50, us_constitution_1787__originalist_reading, suppression_requirement, 50, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_1787__originalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_1787__originalist_reading, 0.18).
narrative_ontology:affects_constraint(us_constitution_1787__originalist_reading, us_constitution_1787__living_reading).
narrative_ontology:affects_constraint(us_constitution_1787__originalist_reading, us_constitution_1787__positivist_reading).
narrative_ontology:affects_constraint(us_constitution_1787__originalist_reading, regulatory_takings_doctrine).
narrative_ontology:affects_constraint(us_constitution_1787__originalist_reading, commerce_clause_interpretation).
narrative_ontology:affects_constraint(us_constitution_1787__originalist_reading, unenumerated_rights_jurisprudence).

% DUAL FORMULATION NOTE:
% The us_constitution_1787 kernel decomposes into three constraint stories: originalist_reading (this file), living_reading (aspirational-framework reading), and positivist_reading (text-plus-amendments reading). All three share the same referent (the Constitution as a governing artifact) but instantiate different ε values and beneficiary/victim structures because they read the Constitution's meaning-determination method differently. Originalism's high extraction reflects its systematic foreclosure of social-rights claims unless they link to 1787 evidence. Living constitutionalism's extraction would be lower (claims need only contemporary moral support). Positivism's extraction would vary by which amendments are read as implicit vs. explicit. The network links all three because they are in constant competition for institutional legitimacy; a shift in one affects the others' operating environment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_1787__originalist_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
