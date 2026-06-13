% ============================================================================
% CONSTRAINT STORY: anthropological_record__creationist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_anthropological_record__creationist_reading, []).

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
 *   constraint_id: anthropological_record__creationist_reading
 *   human_readable: Creationist Reading of the Anthropological Record
 *   domain: epistemology/philosophy_of_science/anthropology
 *
 * SUMMARY:
 *   The creationist reading of the anthropological record asserts that
 *   empirical evidence of human origins is compatible with scriptural
 *   accounts of creation and/or exhibits designed complexity inexplicable by
 *   random mutation and natural selection. This reading emerges as a response
 *   to what creationist communities experience as the monopolization of
 *   origins discourse by scientific materialism. The constraint operates
 *   through school boards, legislation, textbook influence, and community
 *   organizing to assert that creationist and design-theoretic
 *   interpretations deserve equal standing with evolutionary accounts in
 *   educational and public policy contexts. It is CLAIMED as tangled_rope
 *   (combining genuine religious epistemological commitment with
 *   institutional power dynamics) while the authored metrics reflect an
 *   arrangement that has become increasingly extractive and theatrical over
 *   its interval: extractiveness rising from 0.42 to 0.68 (44% increase),
 *   theater rising from 0.28 to 0.52 (86% increase), suppression rising from
 *   0.54 to 0.71 (31% increase). The rise in theater suggests that the
 *   constraint's institutional maintenance increasingly depends on
 *   performative activities (defending the reading in politically sympathetic
 *   forums) rather than on advancing the reading's actual empirical case.
 *
 * KEY AGENTS:
 *   - creationist_religious_communities: Organized religious constituencies (fundamentalist Protestant, some conservative Catholic, some evangelical Christian communities) who hold scriptural authority as binding on how origins are understood and interpreted; agenda-setters for the creationist reading through school boards and legislative advocacy.
 *   - intelligent_design_advocates: Credentialed and semi-credentialed intellectuals (William Dembski, Michael Behe, Discovery Institute) who frame design-theoretic arguments as alternative to materialism; institutional middle ground between literalism and naturalism.
 *   - evolutionary_biologists and paleanthropologists: Institutional scientists whose career authority and disciplinary consensus rest on evolutionary framework; experience the creationist reading as delegitimization and institutional interference.
 *   - secular naturalists and scientific credentialists: Individuals and institutions committed to methodological naturalism and science's epistemic authority; experience the creationist reading as erosion of their institutional power over curriculum and policy.
 *   - public_school_systems: Caught between incompatible mandates (teach robust science, accommodate religious parents) and payers of the conflict's institutional costs (compromise curricula, administrative overhead, litigation).
 *   - indigenous_knowledge_holders: Observers occupying a third epistemological position (relational, oral-transmitted, place-based origins accounts) largely invisible and conscripted by both sides.
 *   - theistic_evolutionists: Excluded from the creationist reading's claim that faith and evolution are opposed; their existence challenges the binary the constraint enforces.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(anthropological_record__creationist_reading, 0.68).
domain_priors:suppression_score(anthropological_record__creationist_reading, 0.71).
domain_priors:theater_ratio(anthropological_record__creationist_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(anthropological_record__creationist_reading, tangled_rope).
narrative_ontology:human_readable(anthropological_record__creationist_reading, "Creationist Reading of the Anthropological Record").
narrative_ontology:topic_domain(anthropological_record__creationist_reading, "epistemology/philosophy_of_science/anthropology").

domain_priors:requires_active_enforcement(anthropological_record__creationist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(anthropological_record__creationist_reading, '76bdb1a5-16d1-418d-85a4-31651e152960').
narrative_ontology:cs_kernel_codification('76bdb1a5-16d1-418d-85a4-31651e152960', distributed).
narrative_ontology:cs_authority_grounding('76bdb1a5-16d1-418d-85a4-31651e152960', extraction).
narrative_ontology:cs_reading_relation('76bdb1a5-16d1-418d-85a4-31651e152960', anthropological_record__naturalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('76bdb1a5-16d1-418d-85a4-31651e152960', anthropological_record__indigenous_epistemology_reading, influences).
narrative_ontology:cs_axiom('76bdb1a5-16d1-418d-85a4-31651e152960', foundational, scriptural_divine_causation_required).
narrative_ontology:cs_axiom_status(scriptural_divine_causation_required, holdable).
narrative_ontology:cs_axiom_grounding('76bdb1a5-16d1-418d-85a4-31651e152960', scriptural_divine_causation_required, deontological).
narrative_ontology:cs_axiom('76bdb1a5-16d1-418d-85a4-31651e152960', foundational, design_complexity_irreducible_to_materialism).
narrative_ontology:cs_axiom_status(design_complexity_irreducible_to_materialism, holdable).
narrative_ontology:cs_axiom_grounding('76bdb1a5-16d1-418d-85a4-31651e152960', design_complexity_irreducible_to_materialism, empirically_contingent).
narrative_ontology:cs_reference_frame('76bdb1a5-16d1-418d-85a4-31651e152960', scriptural_authority_and_human_specialness).
narrative_ontology:cs_drift_state('76bdb1a5-16d1-418d-85a4-31651e152960', contemporary_scientific_materialism_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('76bdb1a5-16d1-418d-85a4-31651e152960', '').
narrative_ontology:cs_kernel_id(anthropological_record__creationist_reading, anthropological_record).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(anthropological_record__creationist_reading, creationist_religious_communities).
narrative_ontology:constraint_victim(anthropological_record__creationist_reading, secular_naturalists).
narrative_ontology:constraint_victim(anthropological_record__creationist_reading, scientific_credentialists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(anthropological_record__creationist_reading, evolutionary_biologists).
narrative_ontology:constraint_victim(anthropological_record__creationist_reading, paleanthropologists_and_geneticists).
narrative_ontology:constraint_victim(anthropological_record__creationist_reading, public_school_systems).
narrative_ontology:constraint_victim(anthropological_record__creationist_reading, textbook_publishers).
narrative_ontology:constraint_vindicates(anthropological_record__creationist_reading, divine_causal_agency).
narrative_ontology:constraint_vindicates(anthropological_record__creationist_reading, scriptural_literal_historicity).
narrative_ontology:constraint_vindicates(anthropological_record__creationist_reading, irreducible_design_complexity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Religious communities that hold a literal or metaphorically-informed reading of scriptural creation accounts. They benefit from the creationist reading because it legitimates their identity, epistemology, and worldview within public discourse and educational policy; it asserts their interpretive authority over how origins are taught. They actively set and defend the reading through school board advocacy, legislative lobbying, and community organizing. Exit would mean renouncing core identity claims about the world's and humanity's origins.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, creationist_religious_communities, beneficiary,
    organized, civilizational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(anthropological_record__creationist_reading, creationist_religious_communities, agenda_setter).

% Credentialed or semi-credentialed advocates who argue for design-theoretic readings of complexity patterns in biology and anthropology. They occupy an institutional middle ground: not claiming direct scriptural literalism but asserting that materialist explanation is insufficient and that intentional design is detectible in the record. They argue for disciplinary pluralism and critique the naturalist monopoly on adjudication.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, intelligent_design_advocates, agenda_setter,
    moderate, generational, mobile, national).

% Credentialed scientists whose disciplinary practice and career authority depend on the materialist evolutionary framework. They bear the cost of institutional contestation — school curricula compromised, funding diverted to ID research, their expertise challenged in public forums despite peer consensus. Their exit is constrained by disciplinary identity and institutional position.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, evolutionary_biologists, payer,
    institutional, generational, constrained, global).

% Scientists producing empirical evidence on human evolutionary origins, genetic ancestry, and antiquity of anatomically modern humans. They are payers because their research is delegitimized as 'atheist materialist assumption' by creationist framings, and their findings are contestable in public/policy discourse despite technical consensus. Their ability to set the curriculum or funding agenda is diminished by the creationist reading's counter-assertion.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, paleanthropologists_and_geneticists, payer,
    institutional, generational, constrained, global).

% Individuals and organizations committed to methodological naturalism and critical of religious authority over epistemic claims. They bear the cost of institutional compromise in education — creation science or ID appearing alongside evolution in curricula, equal-time mandates, and the reframing of empirical questions as 'religious versus secular worldview' rather than empirical versus non-empirical. Their constraint exit is constrained by living in polities where creationist communities have political power.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, secular_naturalists, payer,
    powerful, biographical, constrained, national).

% Institutions and communities claiming epistemic authority via scientific method and peer review. They bear a cost from the creationist reading because it asserts that scientific credentialing is one interpretive tradition among many, not an adjudicative monopoly. Their institutional authority to define curriculum and policy is diminished in jurisdictions where creationist communities have political organizing power.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, scientific_credentialists, payer,
    institutional, generational, constrained, global).

% School systems navigating contradictory mandates: teach empirically robust science, accommodate religious parents' beliefs, avoid entanglement-clause litigation. They pay through curricular compromise (teaching both evolution and creation or ID), administrative overhead managing the controversy, and inability to teach evolutionary biology without contentious public engagement. They are partly payers and partly excluded because they are operated by elected boards responsive to creationist constituencies but excluded from adjudicating what counts as knowledge.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, public_school_systems, payer,
    organized, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(anthropological_record__creationist_reading, public_school_systems, excluded).

% Communities with sustained oral traditions about origins and ancestry distinct from both creationist and naturalist framings. They observe the creationist reading from an epistemological standpoint that neither adopts scriptural Christianity nor privileges materialist science. Their position is neither directly benefited nor harmed by the creationist-naturalist contest, though they are often conscripted into both sides' rhetoric.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, indigenous_knowledge_holders, observer,
    moderate, civilizational, constrained, local).

% Religious individuals and theologians who accept evolutionary science as empirically sound and compatible with faith commitments. They are excluded from the creationist reading's framing (which asserts incompatibility between faith and evolution) and from the naturalist reading's framing (which treats religious identity as epistemically irrelevant). Their position challenges the binary the creationist reading enforces.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, theistic_evolutionists, excluded,
    moderate, biographical, mobile, national).

% Commercial publishers producing educational materials. They bear the cost of producing regional variants, hedging language about evolution, and managing political controversy over what appears in books. Their market and institutional flexibility are constrained by the creationist reading's capacity to organize purchasing power and legislative action.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, textbook_publishers, payer,
    powerful, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(anthropological_record__creationist_reading, creationist_religious_communities).
narrative_ontology:fixing_cost_class(anthropological_record__creationist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework by which religious communities can sustain scriptural authority over fundamental questions (origins, design, human place in creation) against claims that scientific materialism has monopolized those questions. Coordinates communities around shared interpretive premises that the natural record is readable as evidence of intentional design and divine causal agency.
% TRANSFER_FUNCTION: Transfers epistemic authority from scientific credentialists and naturalist interpreters of the record to religious communities and design-theoretic interpreters. Moves policy-setting capacity in education from science curricula boards to school boards responsive to creationist constituencies. Moves public legitimacy from 'evolution is settled science' to 'origins are contested and science is one interpretation among others.'
% ABSENT_VOICES: Indigenous knowledge holders whose non-Western epistemologies of origins are conscripted into both creationist and naturalist arguments but whose actual frameworks (relational, place-based, oral-transmitted) are largely invisible in the public contest. Theistic evolutionists whose position challenges the binary (faith-compatible-with-science) are excluded from the creationist framing's claim that faith and materialism are opposed.
% DISAPPEARANCE_RATIONALE: If the creationist reading vanished overnight—if it became culturally impossible to assert creationist interpretations of the record or to contest evolution on scriptural grounds—public school curricula would stabilize around evolutionary biology without equal-time mandates, textbooks would cease regional variants hedging evolution, and religious epistemology would retreat from public adjudication of empirical questions. The institutional infrastructure built to defend the reading (school board organizing, legislative lobbying, curriculum warfare) would dissolve or redirect. The reading's disappearance would be experienced as a loss of epistemic power by religious communities and a victory by naturalists.
% FOUNDING_PROBLEM: Creationist communities experienced modernist science (Darwinism, deep time, genetic ancestry) as delegitimizing scriptural authority and reducing humans to material accidents. The creationist reading emerged as an attempt to defend scriptural historicity and human specialness against what was interpreted as materialist nihilism. A secondary founding problem: the perception that naturalist science was claiming adjudicative monopoly over questions that religious traditions had historically answered, violating religious communities' autonomy to interpret the world on their own terms.
% FOUNDING_PROBLEM_CORROBORATION: Creationist communities testify that the founding problem remains live: scientific materialism is still presented as the only legitimate framework for understanding origins, and religious interpretation is still marginalized in public institutions. Naturalist scientists and secular educators testify that the founding problem was a misreading: science never claimed to answer 'why' or 'meaning,' only 'how'; religious communities were always free to interpret origins religiously, but not free to substitute creationism for science in science curricula. Theistic evolutionists and scholars of science and religion (from outside both communities) attest that the binary (faith vs. science) misframes the actual relationship and that the founding problem arose from a particular modernist/fundamentalist framing rather than from science itself.
narrative_ontology:disappearance_verdict(anthropological_record__creationist_reading, world_rearranges).
narrative_ontology:founding_problem_status(anthropological_record__creationist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(anthropological_record__creationist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(anthropological_record__creationist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(anthropological_record__creationist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(anthropological_record__creationist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(anthropological_record__creationist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high and rising (0.68 at end) because the creationist reading operates through political power to suppress and subordinate the naturalist reading's institutional standing—not through persuasion in open epistemic competition, but through school board organizing, legislative Equal Time bills, and funding diversion. Suppression (0.71) reflects the enforcement machinery required to keep this reading alive: it must actively exclude rival interpretations from public pedagogy, suppress the empirical record's fit to evolutionary timeline, and maintain a rhetorical boundary between 'faith' and 'science' that is not empirically grounded. Theater (0.52) is moderate-high, reflecting that a growing share of creationist institutional activity is performative (defending the reading in politically sympathetic forums, repeating 'evolution is just a theory' rhetoric) rather than advancing novel empirical arguments. The measurement series runs on one shared time grid (0, 8, 16, 24, 32 within interval 0-32), so every metric is authored at every examined time point. Resistance is high (0.74) because naturalist scientists, evolutionary biologists, paleanthropologists, and secular educators mount substantial technical and institutional opposition to the creationist reading—it does not enjoy frictionless acceptance within credentialed science. Accessibility collapse is moderate (0.48) because alternatives to the creationist reading remain available to most stakeholders: one can remain religious without accepting creationism (theistic evolution), one can study human origins without accepting literalism (most scientists), one can honor indigenous traditions without choosing between creationism and naturalism—the creationist frame does not fully foreclose alternatives, though it claims to.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (naturalist scientists, scientific credentialists, public schools) experience this constraint very differently from the beneficiary seat (creationist communities). From the naturalist seat, the constraint is a snare: it uses religious rhetoric as cover for political power-seeking, it suppresses legitimate science, and it extracts institutional resources through litigation and political theater. From the creationist seat, the constraint is a rope: it defends religious autonomy against materialist monopoly, it coordinates religious communities around shared interpretive authority, and it claims (sincerely from within that frame) to be advancing genuine epistemic pluralism. The engine computes directionality from the structural data: beneficiaries have low d (get epistemic authority without running most science), victims have high d (lose authority and face institutional suppression). The author claims tangled_rope because both coordination (defending religious epistemology) and extraction (suppressing naturalist alternatives) are genuinely present in the constraint's operation; the metrics reflect that extraction has grown while coordination's functional necessity has become questionable (creationist communities could sustain their epistemology in private contexts; the institutional contestation is about public authority).
 *
 * DIRECTIONALITY LOGIC:
 *   Creationist religious communities benefit structurally: they gain epistemic legitimacy for their reading in public policy, curriculum influence, and the reframing of origins from 'settled science' to 'contested between worldviews.' Their power is moderate-to-organized (they can mobilize school boards), their exit is identity-locked (leaving creationism means renouncing core faith commitments), and their directionality is near the beneficiary end (d~0.2). Naturalist scientists and credentialists are targets: they lose institutional authority, face curriculum compromise, and must engage in defensive litigation. Their power is institutional but constrained by political geography, their exit is constrained by disciplinary identity, and their directionality is near the target end (d~0.8). Intelligent design advocates sit in the middle: they benefit from the creationist reading's institutional power but do not directly control it and could exit to pure philosophy without losing career standing (d~0.45). Public schools and textbook publishers are payers forced to compromise and incur overhead without meaningful benefits (d~0.75). The directionality derivation chains directly from the beneficiary/victim declaration: beneficiaries collect epistemic authority and institutional standing, victims lose it; the constraint's persistence depends on suppressing alternatives, not on participant preference for the arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem the creationist reading was built to solve is the perception that scientific materialism delegitimizes religious authority over origins and reduces humans to accidents. That founding problem remains CONTESTED: creationist communities testify it is live (science still marginalizes faith-based interpretation), naturalists testify it is dead (science never claimed to answer meaning, only mechanism). The disappearance verdict is WORLD_REARRANGES: if the creationist reading vanished, curricula would stabilize, textbook variants would cease, and institutional power would shift back to naturalist frameworks. This mismatch (contested founding problem + world-rearranges verdict) indicates potential mandatrophy: the constraint persists not because the founding problem is live, but because institutional power and community identity now ride on the reading's maintenance. The theater rising from 0.28 to 0.52 supports this: performative activity replacing epistemic substance. MANDATROPHY IS NOT RESOLVED for this constraint because the founding problem remains genuinely contested (it is not clearly dead, only disputed); however, the trajectory suggests that creationist institutional organizing is increasingly defensive and theatrical rather than advancing the reading's actual epistemic case. A natural next measurement point would be to track (a) whether extractiveness continues rising (suggesting the constraint is becoming pure snare), (b) whether theater continues rising above 0.6 (indicating the epistemic function has atrophied), and (c) whether creationist community attachment remains tied to faith or has drifted to political identity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Is this constraint a genuine alternate reading of the anthropological record (the creationist interpretation), or is it a political movement using the record as a proxy for defending religious authority against modernist encroachment?',
    'Distinguish the constraint''s epistemic content (how it reads the record) from its institutional function (how it organizes religious power). An empirical reading-only would involve argumentation about genetic data, fossil timing, and design complexity; an institutional-function reading would involve analysis of school board organizing, legislative strategy, and community mobilization. Both may be true simultaneously.',
    'If this is primarily an epistemic reading, classify as a genuine alternate constraint (a different reading of the same kernel). If primarily institutional, reclassify as a snare riding on creationist rhetoric for political gain. If both (likely), emit separate constraint stories: creationist_epistemic_reading (the reading of the record) and creationist_institutional_movement (the political organizing using the reading as cover).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether this constraint is fundamentally about how to interpret empirical evidence or about defending religious institutional power.').

omega_variable(
    design_detectability_ambiguity,
    'Is design complexity empirically detectible in the anthropological record, or does ''design'' exist only as an interpretive frame imposed on the record?',
    'The Intelligent Design movement attempted to develop design-detection metrics (specified complexity, irreducible complexity). These have not achieved peer consensus in biological science. Resolution requires either: (a) empirical metrics for design that replicate across independent researchers, or (b) acceptance that ''design'' is a metaphysical commitment, not an empirical prediction.',
    'If design is empirically detectible, the creationist reading is a genuine epistemic competitor to naturalism. If design is a metaphysical overlay, the creationist reading''s extraction mechanism becomes clearer: it uses apparently empirical language (design complexity) to assert metaphysical premises (intentional agency) that naturalism excludes. The classification may remain tangled_rope either way, but the nature of the extraction changes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(design_detectability_ambiguity, empirical, 'Whether ''design'' is an empirically measurable property or a hermeneutical frame.').

omega_variable(
    scriptural_timeline_empirical_status,
    'Are the scriptural timelines for creation and human origins empirically compatible with the paleontological, genetic, and archaeological records?',
    'Direct comparison of scriptural genealogies (Ussher chronology: ~4000 BCE for creation) with radiometric dating, genetic divergence times (MRCA estimates), and archaeological strata. The empirical record overwhelmingly contradicts a literal recent-creation reading (humans ~200,000 years, anatomically modern humans ~300,000 years, vs. Ussher''s ~6,000 years).',
    'If timelines are empirically incompatible, the creationist reading requires rejecting standard dating methods or invoking metaphorical/gap-theoretic readings of scripture. Either choice increases the constraint''s extractiveness: rejecting dating methods suppresses legitimate scientific alternatives; invoking metaphorical readings suppresses literal scriptural reading. If timelines are somehow made compatible, this omega is resolved and the creationist reading gains epistemic legitimacy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scriptural_timeline_empirical_status, empirical, 'Compatibility of scriptural versus empirical timelines for human origins.').

omega_variable(
    religious_autonomy_vs_epistemic_monopoly,
    'Is the creationist reading defending genuine religious autonomy to interpret the world religiously, or is it claiming religious authority should override scientific pedagogy in secular public institutions?',
    'Separate two claims: (1) Religious communities have the right to teach creationism in private/faith contexts (autonomy claim). (2) Public schools should teach creation science or ID as legitimate alternatives to evolution (authority-over-pedagogy claim). Autonomy can be honored while rejecting authority-over-pedagogy; the creationist reading conflates them to maximize institutional power.',
    'If the constraint is fundamentally about autonomy, the extraction is lower: religious communities can sustain their reading in their own communities without suppressing naturalism. If the constraint extends to public pedagogy, the extraction is higher: it requires suppressing or subordinating scientific framing in contexts that don''t belong to religious communities. The current empirical profile (suppression 0.71, theater 0.52) suggests the constraint is operating on the second register.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(religious_autonomy_vs_epistemic_monopoly, conceptual, 'Whether the constraint seeks religious autonomy or institutional supremacy over science education.').

omega_variable(
    materialist_methodology_vs_materialist_metaphysics,
    'Does the creationist reading oppose scientific methodology (methodological naturalism) or does it oppose metaphysical materialism? Are these the same thing?',
    'Methodological naturalism: science explains natural phenomena via natural causes, not divine intervention—a procedural rule for how to do science. Metaphysical materialism: ultimate reality is material/physical; mind and intention do not exist at fundamental levels—a metaphysical claim. The creationist reading often treats these as identical, claiming that science''s refusal to invoke divine causation proves scientists are materialist metaphysicians. But they can be separated: one can accept methodological naturalism (God doesn''t intervene in the causal order that science studies) while rejecting metaphysical materialism (God and intention exist).',
    'If they are genuinely distinct, the creationist reading''s critique of science is based on a category error, and its extraction mechanism becomes clearer: it suppresses the distinction to create an apparent conflict between faith and science where methodological separation would suffice. If they cannot be separated (methodological naturalism implies metaphysical materialism), the creationist reading has a legitimate point. Theistic evolutionists testify the former; some philosophers of science debate the latter.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(materialist_methodology_vs_materialist_metaphysics, conceptual, 'Whether methodological naturalism logically entails metaphysical materialism.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the measured suppression (0.71) structural (legal exclusion, funding denial, curriculum mandates) or internalized (scientists self-censoring, accepting materialist premises as unchallengeable)?',
    'Post-exit trajectory: if a scientist leaves a context where creationist reading opposes their work and ceases experiencing suppression, the suppression was primarily structural (institutional rules, funding gatekeeping). If suppression persists after exit—internalized doubt about evolutionary claims, habitual materialism—the suppression is partially internalized. Empirical measurement of both components.',
    'If structural, the constraint is primarily institutional (schools, textbooks, funding bodies); fixing it requires policy change. If internalized, the constraint''s effectiveness rests on captured cognitive frames within naturalist communities themselves (scientists who internalize the binary and treat creationism as too illegitimate to engage with); fixing it requires decolonizing epistemology within science itself. The measured suppression of 0.71 likely spans both; the distribution matters for remedies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Structural versus internalized components of the measured suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(anthropological_record__creationist_reading, 0, 32).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anth_tr_t0, anthropological_record__creationist_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(anth_tr_t0, observed).
narrative_ontology:measurement(anth_tr_t8, anthropological_record__creationist_reading, theater_ratio, 8, 0.34).
narrative_ontology:measurement_basis(anth_tr_t8, observed).
narrative_ontology:measurement(anth_tr_t16, anthropological_record__creationist_reading, theater_ratio, 16, 0.41).
narrative_ontology:measurement_basis(anth_tr_t16, observed).
narrative_ontology:measurement(anth_tr_t24, anthropological_record__creationist_reading, theater_ratio, 24, 0.48).
narrative_ontology:measurement_basis(anth_tr_t24, observed).
narrative_ontology:measurement(anth_tr_t32, anthropological_record__creationist_reading, theater_ratio, 32, 0.52).
narrative_ontology:measurement_basis(anth_tr_t32, observed).

% Extraction over time
narrative_ontology:measurement(anth_be_t0, anthropological_record__creationist_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(anth_be_t0, observed).
narrative_ontology:measurement(anth_be_t8, anthropological_record__creationist_reading, base_extractiveness, 8, 0.51).
narrative_ontology:measurement_basis(anth_be_t8, observed).
narrative_ontology:measurement(anth_be_t16, anthropological_record__creationist_reading, base_extractiveness, 16, 0.59).
narrative_ontology:measurement_basis(anth_be_t16, observed).
narrative_ontology:measurement(anth_be_t24, anthropological_record__creationist_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement_basis(anth_be_t24, observed).
narrative_ontology:measurement(anth_be_t32, anthropological_record__creationist_reading, base_extractiveness, 32, 0.68).
narrative_ontology:measurement_basis(anth_be_t32, observed).

% Suppression requirement over time
narrative_ontology:measurement(anth_su_t0, anthropological_record__creationist_reading, suppression_requirement, 0, 0.54).
narrative_ontology:measurement_basis(anth_su_t0, observed).
narrative_ontology:measurement(anth_su_t8, anthropological_record__creationist_reading, suppression_requirement, 8, 0.59).
narrative_ontology:measurement_basis(anth_su_t8, observed).
narrative_ontology:measurement(anth_su_t16, anthropological_record__creationist_reading, suppression_requirement, 16, 0.64).
narrative_ontology:measurement_basis(anth_su_t16, observed).
narrative_ontology:measurement(anth_su_t24, anthropological_record__creationist_reading, suppression_requirement, 24, 0.68).
narrative_ontology:measurement_basis(anth_su_t24, observed).
narrative_ontology:measurement(anth_su_t32, anthropological_record__creationist_reading, suppression_requirement, 32, 0.71).
narrative_ontology:measurement_basis(anth_su_t32, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(anthropological_record__creationist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(anthropological_record__creationist_reading, 0.12).
narrative_ontology:affects_constraint(anthropological_record__creationist_reading, anthropological_record__naturalist_reading).
narrative_ontology:affects_constraint(anthropological_record__creationist_reading, anthropological_record__indigenous_epistemology_reading).

% DUAL FORMULATION NOTE:
% The anthropological_record kernel has three readings: creationist_reading (this story, ε~0.68), naturalist_reading (ε~0.15, nearly mountain), and indigenous_epistemology_reading (ε~0.35). The creationist reading's high extraction stems from its requirement to suppress and subordinate rival readings in public institutions; the naturalist reading's low extraction reflects near-complete institutional dominance in credentialed science; the indigenous reading's moderate extraction reflects systematic marginalization despite epistemological legitimacy. These are NOT observational angles on one constraint; they are three structurally distinct constraints on how the anthropological record is read and what counts as authority to answer origins questions. Decomposition follows the ε-invariance principle: changing the observer (or measurement framework) changes ε; if ε changes, there are two constraints, not one observation angle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(anthropological_record__creationist_reading, organized, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
