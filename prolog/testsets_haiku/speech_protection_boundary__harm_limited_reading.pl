% ============================================================================
% CONSTRAINT STORY: speech_protection_boundary__harm_limited_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_boundary__harm_limited_reading, []).

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
 *   constraint_id: speech_protection_boundary__harm_limited_reading
 *   human_readable: Speech Protection Conditional on Harm-Absence (Dignity/Equality/Harassment Frame)
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested kernel
 *   'speech_protection_boundary.' The harm-limited reading narrows the
 *   protected set by excluding speech that causes significant harm to
 *   dignity, equality, and freedom from harassment. This reading holds that
 *   equal dignity is itself a prerequisite for equal speech protection — that
 *   speakers cannot stand on equal footing when some categories of speech
 *   systematically construct the social inferiority of historically
 *   marginalized groups. The reading requires active state gatekeeping to
 *   distinguish protected from unprotected speech, creating the institutional
 *   vulnerability that exclusionary voices warn about. The constraint is
 *   claimed as tangled_rope because it possesses BOTH a genuine coordination
 *   function (pluralistic society needs some rule for distributing speech
 *   authority and harm-bearing) AND asymmetric extraction (the gatekeeper
 *   gains power over discourse, speakers of edge cases bear uncertainty
 *   costs, and the boundary's discretionary nature creates abuse risk). This
 *   reading stands in a structural relationship to its sibling readings: it
 *   forecloses the absolutist reading's premise (near-absolute speech
 *   protection) within any single coherent framework, but coexists with the
 *   balancing reading as a live position held by different institutional
 *   actors and schools of constitutional thought.
 *
 * KEY AGENTS:
 *   - marginalized_dignity_bearers (powerless, trapped, generational) — benefit from the constraint; their dignity is the vindicated proposition
 *   - equality_enforcement_bodies (institutional, gatekeeper) — set and enforce the harm boundary; gain authority and discretion
 *   - speakers_subject_to_gatekeeper_review (moderate, constrained, biographical) — bear the direct extraction; face speech restriction and professional consequences
 *   - speakers_with_coded_or_edge_expression (moderate, identity_locked, biographical) — bear extraction through epistemic uncertainty; their identity fuses with modes of expression
 *   - state_speech_gatekeepers (institutional, dual-positioned) — administer the boundary; gain discretionary power but also bear cost of managing abuse risk and appeals
 *   - absolutist_speech_defenders (organized, excluded) — would object that gatekeeper power is a worse harm than ambient dignity threat
 *   - balancing_framework_advocates (organized, excluded) — argue this reading abandons principled weighing in favor of dignity as per-se trump
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__harm_limited_reading, 0.68).
domain_priors:suppression_score(speech_protection_boundary__harm_limited_reading, 0.71).
domain_priors:theater_ratio(speech_protection_boundary__harm_limited_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__harm_limited_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_boundary__harm_limited_reading, "Speech Protection Conditional on Harm-Absence (Dignity/Equality/Harassment Frame)").
narrative_ontology:topic_domain(speech_protection_boundary__harm_limited_reading, "constitutional/political").

domain_priors:requires_active_enforcement(speech_protection_boundary__harm_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__harm_limited_reading, '87d77ae4-8fd3-479d-88f2-6c1a7e81fcf4').
narrative_ontology:cs_kernel_codification('87d77ae4-8fd3-479d-88f2-6c1a7e81fcf4', fixed_text).
narrative_ontology:cs_authority_grounding('87d77ae4-8fd3-479d-88f2-6c1a7e81fcf4', extraction).
narrative_ontology:cs_interpretation_layer_present('87d77ae4-8fd3-479d-88f2-6c1a7e81fcf4').
narrative_ontology:cs_reading_relation('87d77ae4-8fd3-479d-88f2-6c1a7e81fcf4', speech_protection_boundary__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('87d77ae4-8fd3-479d-88f2-6c1a7e81fcf4', speech_protection_boundary__balancing_reading, coexists_with).
narrative_ontology:cs_axiom('87d77ae4-8fd3-479d-88f2-6c1a7e81fcf4', foundational, equal_dignity_prerequisite_for_equal_speech).
narrative_ontology:cs_axiom_status(equal_dignity_prerequisite_for_equal_speech, holdable).
narrative_ontology:cs_axiom_grounding('87d77ae4-8fd3-479d-88f2-6c1a7e81fcf4', equal_dignity_prerequisite_for_equal_speech, deontological).
narrative_ontology:cs_axiom('87d77ae4-8fd3-479d-88f2-6c1a7e81fcf4', foundational, dignity_harm_cognizable_speech_interest).
narrative_ontology:cs_axiom_status(dignity_harm_cognizable_speech_interest, holdable).
narrative_ontology:cs_axiom_grounding('87d77ae4-8fd3-479d-88f2-6c1a7e81fcf4', dignity_harm_cognizable_speech_interest, deontological).
narrative_ontology:cs_reference_frame('87d77ae4-8fd3-479d-88f2-6c1a7e81fcf4', formalized_equal_protection_doctrine).
narrative_ontology:cs_drift_state('87d77ae4-8fd3-479d-88f2-6c1a7e81fcf4', contemporary_diversity_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('87d77ae4-8fd3-479d-88f2-6c1a7e81fcf4', '').
narrative_ontology:cs_kernel_id(speech_protection_boundary__harm_limited_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__harm_limited_reading, marginalized_dignity_bearers).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__harm_limited_reading, equality_principle_vindicators).
narrative_ontology:constraint_victim(speech_protection_boundary__harm_limited_reading, speech_speakers_subject_to_gatekeeper_review).
narrative_ontology:constraint_victim(speech_protection_boundary__harm_limited_reading, speakers_with_coded_or_edge_expression).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(speech_protection_boundary__harm_limited_reading, speakers_subject_to_gatekeeper_review).
narrative_ontology:constraint_victim(speech_protection_boundary__harm_limited_reading, state_speech_gatekeepers).
narrative_ontology:constraint_vindicates(speech_protection_boundary__harm_limited_reading, equal_dignity_as_constitutional_prerequisite).
narrative_ontology:constraint_vindicates(speech_protection_boundary__harm_limited_reading, harassment_harm_as_cognizable_interest).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Members of groups historically targeted by dehumanizing slurs, epithets, and coded language (racial minorities, religious minorities, LGBTQ+ persons, immigrants). They experience ambient targeted speech as a system of epistemic invalidation and social subordination. The constraint frames their dignity as a constitutional interest; they gain a claim to protection from speech that the law treats as constructing their inferiority. They cannot exit their group status or leave citizenship without extreme cost.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, marginalized_dignity_bearers, beneficiary,
    powerless, generational, trapped, national).

% Courts applying equal protection doctrine, civil rights agencies, legislatures enacting anti-discrimination and anti-harassment law. They set the boundary between protected and unprotected speech, determine what counts as significant harm to dignity and equality, and enforce through judicial decision, agency action, and statute. They hold the definitional power and enforcement power; they collect the authority that the constraint distributes.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, equality_enforcement_bodies, agenda_setter,
    institutional, generational, analytical, national).

% Individuals whose speech falls within the unprotected set as defined by this reading: those uttering statements classified as hate speech, harassment, or dignity-violating expression. They face platform removal, legal liability, professional consequences, social sanction, and reputation damage. They can exit through self-censorship, code-switching, geographic relocation, or career change; these are costly but possible options.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, speakers_subject_to_gatekeeper_review, payer,
    moderate, biographical, constrained, national).

% Speakers using dog-whistle language, satirical inversion of slurs, academic language on contested topics, artistic expression that skirts the boundary, or the kind of ironic ribbing that belongs to in-group communication. They face uncertainty about whether their expression will be classified as dignity harm or harassment; they bear the cost of anticipatory self-censorship and reclassification risk. Their identity often fuses with their mode of expression (they are artists, satirists, academics in controversial fields, or members of subcultures with specific speech practices). Exit through code-switching or mode-switching is identity-costly.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, speakers_with_coded_or_edge_expression, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_boundary__harm_limited_reading, speakers_with_coded_or_edge_expression, observer).

% Government bodies (courts, agencies, legislative bodies) tasked with implementing the harm-limited boundary. They gain institutional authority to regulate speech and discretionary power to classify utterances. They also bear costs: they must administer speech classification, manage appeals and litigation, resist pressure to weaponize the boundary against political opponents, and maintain public legitimacy for their gatekeeping authority. The directionality override (d=0.55, near-symmetric) reflects this dual position: they gain substantial authority but also carry substantial institutional burden.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, state_speech_gatekeepers, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_boundary__harm_limited_reading, state_speech_gatekeepers, payer).

% Civil liberties organizations, First Amendment maximalists, constitutional scholars and lawyers who believe near-absolute speech protection is required to prevent government abuse. They argue that the dignity gatekeeping apparatus, once established, will inevitably be weaponized against disfavored speech and that equal dignity is better served by equal speech protection than by curated boundaries. Their position that gatekeeper power is a worse harm than ambient speech harm is structurally excluded from the dignity-centered framing of this reading.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, absolutist_speech_defenders, excluded,
    organized, generational, constrained, national).

% Constitutional scholars, judges, and legal theorists who hold that speech restrictions should be determined through explicit case-by-case weighing of competing constitutional interests rather than through categorical rules. They see the harm-limited reading as abandoning principled balancing in favor of dignity as a per-se trump, which they regard as unprincipled gatekeeping. Their view that the boundary should emerge from analysis rather than be imposed ex-ante is excluded from this reading's framework.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, balancing_framework_advocates, excluded,
    organized, generational, constrained, national).

% Judicial bodies (especially appellate courts) responsible for determining which reading of the speech protection boundary is constitutionally required or permitted. They adjudicate individual cases testing the boundary, develop doctrine, and interpret the scope of dignity-based unprotected categories. They carry the epistemic burden of distinguishing dignitary harm from viewpoint discrimination and harassment from disfavored ideas.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, courts_applying_speech_doctrine, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_boundary__harm_limited_reading, equality_enforcement_bodies).
narrative_ontology:fixing_cost_class(speech_protection_boundary__harm_limited_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for speech rights that incorporates equal dignity and freedom from ambient harassment as substantive liberty interests, not secondary to classical First Amendment concerns. Solves the coordination problem of a plural society where some utterances systematically construct the social inferiority of historically marginalized groups: any unified speech regime must distribute both speech access and harm-bearing. This reading opts to constrain some speakers' utterances in order to protect others' dignity and equal citizenship.
% TRANSFER_FUNCTION: Moves speech authority from the speaker (who under absolutist framing retains near-unlimited claim to utter whatever they judge relevant) to the gatekeeper (courts, agencies, institutions) who determines which utterances cause significant harm to dignity, equality, or freedom from harassment. Moves the obligation to silence and self-censor from historically targeted groups (who under ambient harassment bear the cost of managing dehumanizing speech) to speakers whose speech the gatekeeper judges harmful.
% ABSENT_VOICES: Civil libertarians and First Amendment absolutists are systematically excluded from this reading's legitimacy frame: their argument that gatekeeper power itself becomes a worse harm to speech freedom and to the marginalized groups it claims to protect is routed outside the dignity-centered logic. Speakers of edge cases, satirists, and academic speakers on contested topics are not at the table when the harm boundary is defined, though their expression is the subject of classification and potential restriction. The balancing-framework advocates argue for different decisional method but are not treated as having a seat in this reading's framework.
% DISAPPEARANCE_RATIONALE: If this reading's apparatus vanished — if courts returned to near-absolute speech protection and removed institutional discretion to regulate speech on dignity/equality/harassment grounds — the constitutional meaning of equal protection would reorganize. Speaker behavior would shift (coded language might become explicit, marginalized groups would report increased epistemic invalidation and harassment, antidiscrimination law and institutional practices would become the primary locus of equality enforcement). The speech/non-speech boundary for regulating subordination would move, and the institutional distribution of authority over discourse would fundamentally change.
% FOUNDING_PROBLEM: Historically marginalized groups experience targeted dehumanizing speech — slurs, epithets, coded language, dehumanizing comparisons — as a coordinated system of epistemic invalidation and social subordination that constructs their inferiority in the public mind. The law treats most of this speech as protected political expression and places the burden on targeted groups to manage the harm, ignore it, or counter-speak. The founding problem is the gap between formal legal equality (equal protection) and substantive dignity equality (equal standing in speech communities and civic participation).
% FOUNDING_PROBLEM_CORROBORATION: Scholars of subordination, civil rights advocates, and marginalized communities testify that targeted dehumanizing speech persists and causes measurable epistemic and dignity harm. Historians document the role of dehumanizing speech in coordinating violence and subordination. First Amendment scholars and civil libertarians contest this reading's remedy: they argue that speech restriction creates worse institutional and epistemic harms, and that dignity is better protected by non-speech mechanisms (antidiscrimination enforcement, institutional diversity, cultural norm change). No unified corroboration exists outside the beneficiary set; the founding problem itself is contested in legitimacy terms (is it a problem that requires speech remedy, or is it overstated relative to the institutional risks of gatekeeping?).
narrative_ontology:disappearance_verdict(speech_protection_boundary__harm_limited_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_boundary__harm_limited_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_boundary__harm_limited_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(speech_protection_boundary__harm_limited_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_boundary__harm_limited_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_boundary__harm_limited_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_boundary__harm_limited_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at endpoint) reflects the constraint's net effect: marginalized groups gain protection from some speech harms, but speakers lose speech authority to a gatekeeper, and the gatekeeper gains discretionary power that can be weaponized. The asymmetry is real and substantial — the beneficiaries do not run the system, the payers do not set the boundary. Suppression (0.71) is high because the constraint's persistence depends on active enforcement: courts must classify speech, platforms must remove it, speakers must be sanctioned. If enforcement relaxed, the boundary would collapse and speakers would revert to less guarded expression. Theater (0.42) reflects the constraint's performative dimension: some portion of gatekeeper activity is devoted to demonstrating commitment to dignity protection rather than preventing measurable harm (courts issue dramatic condemnations of hate speech, institutions adopt speech codes and then abandon them under legal challenge, etc.). Accessibility_collapse (0.62) reflects that once the harm framing is institutionalized, alternatives partially collapse for speakers — they cannot easily 'exit' the speech standard without geographic or professional relocation. Resistance (0.58) reflects that the constraint meets substantial opposition from First Amendment defenders and speakers, who mount legal challenges, academic critique, and rhetorical resistance. The measurement series shows extraction and suppression rising over the interval as the boundary expands (as case law refines and institutions proliferate speech codes) and then plateauing — the constraint reaches a stable extractive equilibrium rather than continuing to intensify. Theater rises more continuously because performative demonstration of commitment to dignity protection becomes more institutionally salient over time (institutional virtue-signaling around diversity and inclusion). All metrics are authored on a single shared time grid; every metric has a value at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   From the gatekeeper seat (equality_enforcement_bodies, state_speech_gatekeepers), the constraint is legitimate institutional authority protecting fundamental dignity — a coordination solution to a real problem. From the speaker seat (speakers_subject_to_gatekeeper_review), the constraint appears as an extraction mechanism disguised as dignity protection — a loss of speech authority to a actor that gains power from maintaining the boundary. The marginalized_dignity_bearers seat sees the constraint as essential; the absolutist_speech_defenders seat sees it as the camel's nose toward majoritarian suppression. The engine computes these divergent type-classifications from the structural data; the authored claim (tangled_rope) reflects the analytical position that all these seats' experiences are real and that the constraint possesses both genuine coordination and genuine asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Marginalized_dignity_bearers are net beneficiaries (d near 0.0): they gain dignitary protection, cannot exit, and depend on the constraint for equal citizenship. Their low exit_options (trapped, identity_locked) and high vulnerability to speech harm push d downward. Speakers_subject_to_gatekeeper_review are net targets (d near 1.0): they lose speech authority, face sanction, and bear the cost of gatekeeper discretion. Their moderate power and constrained exit options push d upward. Speakers_with_coded_or_edge_expression are also targets but with added burden: their identity is fused with their modes of expression (identity_locked exit), so exit costs are compounded; their d is near 1.0 with amplification from identity-lock. Equality_enforcement_bodies and state_speech_gatekeepers are positioned as agenda-setters (they set and enforce the boundary) but face a secondary cost: they must resist weaponization pressure and manage appeals. The directed-both-ways rider (a seat that both benefits and pays) applies here — the gatekeeper gains authority but also bears the institutional cost of administering and defending the boundary against legal challenge and pressure to expand it into viewpoint discrimination. No directionality overrides are needed; the structural data produces accurate d values across seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits a real coordination function (managing speech authority in a plural society where not all utterances can coexist without harm) paired with real asymmetric extraction (gatekeeper power over discourse, speaker vulnerability to discretion). The tangled_rope classification prevents misconstruing this as either pure coordination (rope, where participants are net beneficiaries) or pure extraction (snare, where the coordination story is pure cover). The omegas document the specific sites of ambiguity: whether the gatekeeper apparatus can be administered without becoming a tool of majoritarian suppression, whether dignity harm is sufficiently determinate to avoid viewpoint discrimination, whether the founding problem (dignity threat from targeted speech) remains live or has been substantially addressed by other mechanisms (antidiscrimination law, institutional representation, cultural shifts). Mandatrophy (mandate outliving function) is not yet present: the constraint's founding problem (equal dignity in the face of dehumanizing speech) remains contested and substantially live, so the mandate has not yet become purely theatrical. However, the rising theater_ratio suggests that some portion of enforcement activity is performative rather than preventive of measurable harm, which is an early warning sign.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gatekeeper_discretion_vs_viewpoint_discrimination,
    'Can courts and institutions reliably distinguish speech that causes significant harm to dignity/equality/harassment from speech that merely expresses disfavored political views? Or does the discretion required to implement the boundary inevitably become a tool for suppressing political opposition?',
    'Longitudinal analysis of court decisions and institutional speech codes: do determinations correlate with the political valence of the speech or with content-neutral harm metrics? Comparative-law evidence from jurisdictions with explicit dignity-based speech restrictions — do their patterns show systematic political weaponization?',
    'If gatekeeper discretion systematically correlates with viewpoint suppression (not just content regulation), the constraint reclassifies from tangled_rope (mixed coordination and extraction) to snare (pure extraction disguised as dignity protection). If discretion tracks harm metrics reliably across political contexts, the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeper_discretion_vs_viewpoint_discrimination, empirical, 'Whether gatekeeper discretion in defining dignity harm remains neutral across political contexts or becomes a tool for viewpoint suppression.').

omega_variable(
    dignity_harm_determinacy,
    'Is ''significant harm to dignity'' a sufficiently determinate standard to guide judicial and institutional decision-making, or is the concept too plastic to avoid circularity (harm to dignity = speech a gatekeeper judges as dignity-violating)?',
    'Analysis of case law: do courts develop stable doctrine distinguishing dignity harm from offense, insult, or disagreement? Can institutional speech codes be applied consistently across time and contexts, or do boundaries shift with changing institutional priorities?',
    'High determinacy would support the constraint as legitimate coordination. Low determinacy would suggest the gatekeeper boundary is essentially arbitrary, which amplifies the extraction and institutional-abuse dimensions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dignity_harm_determinacy, conceptual, 'Whether dignity harm is sufficiently determinate to guide consistent application or whether it remains indeterminate and subject to motivated application.').

omega_variable(
    foundational_problem_live_or_resolved,
    'Is the founding problem (equal dignity under conditions of dehumanizing speech targeting marginalized groups) still live, or have other mechanisms (antidiscrimination law, institutional diversity practices, cultural norm shifts) substantially addressed it such that speech restriction is no longer necessary to achieve equal dignity?',
    'Empirical measurement: do marginalized groups report persistent epistemic invalidation and dignity harm from speech in jurisdictions WITH and WITHOUT this reading''s speech restrictions? Has the introduction of harm-based speech restriction causally reduced reported harm, or has harm persisted/changed form?',
    'If the founding problem is substantially resolved by non-speech mechanisms, the constraint risks mandatrophy — a shell of enforcement persisting after the function has been absorbed. If the problem remains live and speech restriction demonstrably reduces harm, the constraint retains legitimacy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(foundational_problem_live_or_resolved, empirical, 'Whether the dignity-harm problem the constraint was built to address remains live or has been substantially addressed by other institutional developments.').

omega_variable(
    kernel_reading_committer_structure,
    'Is this reading (harm-limited, gatekeeper-dependent) genuinely a stable constitutional principle, or is it functionally a temporary expansion of state speech authority that will revert to absolutist or balancing frameworks when political pressure shifts?',
    'Meta-analysis of constitutional doctrine over time: does this reading survive political transitions? Does it maintain its boundaries or does it shift into less protective or more protective directions depending on which parties control the gatekeeper institutions? Comparative evidence from other democracies: which framings prove stable across changes in political control?',
    'If the reading proves unstable and shifts with political control, it is less a principle than a tool — the constraint exhibits higher extraction and lower coordination value than its legitimacy framing suggests. If stable, it represents a genuine constitutional choice point.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Whether this reading represents a stable constitutional principle or a temporary political-dependent expansion of gatekeeper authority.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.71) structural — external enforcement machinery and institutional sanction — or internalized — speakers have absorbed the boundary into their own judgment and self-censor even without institutional enforcement?',
    'Post-relaxation trajectory: in jurisdictions or subcultures that relax speech enforcement (or where enforcement is unevenly applied), do speakers revert to previously-restricted speech, or do they maintain self-censorship? Do speaker surveys report internalized acceptance of the boundary or ongoing resentment and suppression?',
    'If suppression is primarily structural, its removal would reverse the constraint quickly. If suppression is substantially internalized, speakers carry the constraint''s effect even absent institutional enforcement — exit is compromised even for mobile speakers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether measured suppression is structural (external enforcement) or internalized (absorbed into speaker judgment).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__harm_limited_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_boundary__harm_limited_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(spee_tr_t0, observed).
narrative_ontology:measurement(spee_tr_t8, speech_protection_boundary__harm_limited_reading, theater_ratio, 8, 0.23).
narrative_ontology:measurement_basis(spee_tr_t8, observed).
narrative_ontology:measurement(spee_tr_t16, speech_protection_boundary__harm_limited_reading, theater_ratio, 16, 0.29).
narrative_ontology:measurement_basis(spee_tr_t16, observed).
narrative_ontology:measurement(spee_tr_t24, speech_protection_boundary__harm_limited_reading, theater_ratio, 24, 0.34).
narrative_ontology:measurement_basis(spee_tr_t24, observed).
narrative_ontology:measurement(spee_tr_t32, speech_protection_boundary__harm_limited_reading, theater_ratio, 32, 0.39).
narrative_ontology:measurement_basis(spee_tr_t32, observed).
narrative_ontology:measurement(spee_tr_t40, speech_protection_boundary__harm_limited_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(spee_tr_t40, observed).
narrative_ontology:measurement(spee_tr_t50, speech_protection_boundary__harm_limited_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement_basis(spee_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_boundary__harm_limited_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(spee_be_t0, observed).
narrative_ontology:measurement(spee_be_t8, speech_protection_boundary__harm_limited_reading, base_extractiveness, 8, 0.54).
narrative_ontology:measurement_basis(spee_be_t8, observed).
narrative_ontology:measurement(spee_be_t16, speech_protection_boundary__harm_limited_reading, base_extractiveness, 16, 0.61).
narrative_ontology:measurement_basis(spee_be_t16, observed).
narrative_ontology:measurement(spee_be_t24, speech_protection_boundary__harm_limited_reading, base_extractiveness, 24, 0.65).
narrative_ontology:measurement_basis(spee_be_t24, observed).
narrative_ontology:measurement(spee_be_t32, speech_protection_boundary__harm_limited_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement_basis(spee_be_t32, observed).
narrative_ontology:measurement(spee_be_t40, speech_protection_boundary__harm_limited_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(spee_be_t40, observed).
narrative_ontology:measurement(spee_be_t50, speech_protection_boundary__harm_limited_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(spee_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_boundary__harm_limited_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(spee_su_t0, observed).
narrative_ontology:measurement(spee_su_t8, speech_protection_boundary__harm_limited_reading, suppression_requirement, 8, 0.5).
narrative_ontology:measurement_basis(spee_su_t8, observed).
narrative_ontology:measurement(spee_su_t16, speech_protection_boundary__harm_limited_reading, suppression_requirement, 16, 0.58).
narrative_ontology:measurement_basis(spee_su_t16, observed).
narrative_ontology:measurement(spee_su_t24, speech_protection_boundary__harm_limited_reading, suppression_requirement, 24, 0.64).
narrative_ontology:measurement_basis(spee_su_t24, observed).
narrative_ontology:measurement(spee_su_t32, speech_protection_boundary__harm_limited_reading, suppression_requirement, 32, 0.68).
narrative_ontology:measurement_basis(spee_su_t32, observed).
narrative_ontology:measurement(spee_su_t40, speech_protection_boundary__harm_limited_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement_basis(spee_su_t40, observed).
narrative_ontology:measurement(spee_su_t50, speech_protection_boundary__harm_limited_reading, suppression_requirement, 50, 0.71).
narrative_ontology:measurement_basis(spee_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__harm_limited_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(speech_protection_boundary__harm_limited_reading, 0.12).
narrative_ontology:affects_constraint(speech_protection_boundary__harm_limited_reading, speech_protection_boundary__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_boundary__harm_limited_reading, speech_protection_boundary__balancing_reading).
narrative_ontology:affects_constraint(speech_protection_boundary__harm_limited_reading, institutional_gatekeeper_authority_over_discourse).
narrative_ontology:affects_constraint(speech_protection_boundary__harm_limited_reading, equal_dignity_as_substantive_liberty).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'speech_protection_boundary.' The absolutist_reading and balancing_reading are sibling constraints with different ε values and beneficiary/victim structures. The harm-limited reading forecloses the absolutist reading (core premises are logically contradictory) but coexists with the balancing reading (both live in different courts/jurisdictions). The network edge points to the other readings and to downstream constraints about institutional authority and substantive liberty that instantiate this reading's implications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_protection_boundary__harm_limited_reading, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
