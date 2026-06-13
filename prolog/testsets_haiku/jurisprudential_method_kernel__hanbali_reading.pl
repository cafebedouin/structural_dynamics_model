% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__hanbali_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__hanbali_reading, []).

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
 *   constraint_id: jurisprudential_method_kernel__hanbali_reading
 *   human_readable: Hanbali Textualist Jurisprudential Method: Literal Reading + Consensus Gate
 *   domain: religious/legal/institutional
 *
 * SUMMARY:
 *   This constraint represents the Hanbali school's core jurisprudential
 *   method: law derives exclusively from the literal text of Qur'an,
 *   authenticated Hadith reports, and unanimous Companion consensus.
 *   Analogical reasoning (qiyas) and juristic preference (istihsan) are
 *   rejected as bid'ah—innovation that corrupts the divine law. This is ONE
 *   READING of the contested kernel of Islamic jurisprudential methodology.
 *   The Hanafi, Maliki, and Shafi'i schools read the same kernel (how to
 *   derive law from divine sources) but reach structurally different
 *   conclusions: the Hanbali reading rejects analogical reasoning entirely;
 *   the Hanafi reading elevates it; the Shafi'i reading hierarchizes it; the
 *   Maliki reading grounds law in living community practice. The claim/metric
 *   gap is deliberate: the constraint is CLAIMED as a restoration of pure
 *   textualism (coordinate with divine will, hence coordination function)
 *   while the authored metrics describe substantially extractive operation
 *   (high suppression of rationalist methods, rising theater ratio as the
 *   textualist apparatus becomes more performative over centuries). The
 *   engine measures this divergence; do not reconcile the claim to the
 *   metrics.
 *
 * KEY AGENTS:
 *   - Hanbali textualist scholars: institutional agenda-setters who enforce the method and determine consensus; identity-locked (their authority depends on doctrinal purity).
 *   - Purist hadith transmitters: organized beneficiaries whose scholarly apparatus becomes indispensable when hadith authentication is the primary legitimacy gate.
 *   - Rationalist jurists (Hanafi, some Shafi'i schools): powerful payers constrained by the exclusion of qiyas; forced to either adapt to textualist criteria or forfeit institutional recognition.
 *   - Customary practice communities: organized payers at regional scope; their adaptive jurisprudence is delegitimized unless it achieves unanimous Companion consensus (near-impossible gate).
 *   - Political authorities: institutional agenda-setters and beneficiaries who use the constraint's claim to immutability as a source of legitimacy; they apply textualism selectively for their own interests.
 *   - Islamic legal theorists: analytical observers documenting the constraint's structure and operation across history.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__hanbali_reading, 0.68).
domain_priors:suppression_score(jurisprudential_method_kernel__hanbali_reading, 0.72).
domain_priors:theater_ratio(jurisprudential_method_kernel__hanbali_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__hanbali_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__hanbali_reading, "Hanbali Textualist Jurisprudential Method: Literal Reading + Consensus Gate").
narrative_ontology:topic_domain(jurisprudential_method_kernel__hanbali_reading, "religious/legal/institutional").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__hanbali_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__hanbali_reading, 'e0ef8123-63d0-416d-b2be-376e77f95f37').
narrative_ontology:cs_kernel_codification('e0ef8123-63d0-416d-b2be-376e77f95f37', fixed_text).
narrative_ontology:cs_authority_grounding('e0ef8123-63d0-416d-b2be-376e77f95f37', lineage).
narrative_ontology:cs_interpretation_layer_present('e0ef8123-63d0-416d-b2be-376e77f95f37').
narrative_ontology:cs_reading_relation('e0ef8123-63d0-416d-b2be-376e77f95f37', jurisprudential_method_kernel__hanafi_reading, forecloses).
narrative_ontology:cs_reading_relation('e0ef8123-63d0-416d-b2be-376e77f95f37', jurisprudential_method_kernel__maliki_reading, forecloses).
narrative_ontology:cs_reading_relation('e0ef8123-63d0-416d-b2be-376e77f95f37', jurisprudential_method_kernel__shafii_reading, coexists_with).
narrative_ontology:cs_axiom('e0ef8123-63d0-416d-b2be-376e77f95f37', foundational, qiyas_is_bidah).
narrative_ontology:cs_axiom_status(qiyas_is_bidah, holdable).
narrative_ontology:cs_axiom_grounding('e0ef8123-63d0-416d-b2be-376e77f95f37', qiyas_is_bidah, deontological).
narrative_ontology:cs_axiom('e0ef8123-63d0-416d-b2be-376e77f95f37', foundational, textual_sufficiency_for_all_cases).
narrative_ontology:cs_axiom_status(textual_sufficiency_for_all_cases, holdable).
narrative_ontology:cs_axiom_grounding('e0ef8123-63d0-416d-b2be-376e77f95f37', textual_sufficiency_for_all_cases, empirically_contingent).
narrative_ontology:cs_axiom('e0ef8123-63d0-416d-b2be-376e77f95f37', secondary, companion_consensus_only_unanimity_valid).
narrative_ontology:cs_axiom_status(companion_consensus_only_unanimity_valid, holdable).
narrative_ontology:cs_axiom_grounding('e0ef8123-63d0-416d-b2be-376e77f95f37', companion_consensus_only_unanimity_valid, deontological).
narrative_ontology:cs_reference_frame('e0ef8123-63d0-416d-b2be-376e77f95f37', quranic_hadith_sufficiency).
narrative_ontology:cs_drift_state('e0ef8123-63d0-416d-b2be-376e77f95f37', contemporary_rationalist_challenge, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e0ef8123-63d0-416d-b2be-376e77f95f37', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanbali_reading, hanbali_textualist_scholars).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanbali_reading, purist_hadith_transmitters).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, rationalist_jurists).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, customary_practice_communities).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, analogical_reasoning_proponents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanbali_reading, political_authorities_claiming_hanbali_purity).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__hanbali_reading, divine_text_sufficiency).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__hanbali_reading, innovation_corruption_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Establishes and enforces the methodological constraint that law derives exclusively from literal Qur'anic text, authenticated Hadith reports (primarily those meeting strict transmission criteria), and Companion consensus. Sets the interpretive rules that exclude qiyas (analogical reasoning) and istihsan (juristic preference) as bid'ah. Their institutional authority rests on doctrinal purity and fidelity to an unchanging hermeneutic standard. They administer the consensus gate, determining which opinions count as valid unanimity.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, hanbali_textualist_scholars, agenda_setter,
    institutional, generational, identity_locked, continental).

% Benefit from the constraint's elevation of hadith authentication as the primary legitimacy source. Their specialized knowledge of transmission chains, narrator reliability, and text authentication becomes indispensable to jurisprudence. The constraint directs authority and resources toward their scholarly apparatus.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, purist_hadith_transmitters, beneficiary,
    organized, generational, identity_locked, continental).

% Bear the cost of the constraint's rejection of qiyas and istihsan. They cannot extend the law to novel cases through reasoned analogy from established principles; their preferred methodological tools are declared invalid. They must either adapt their methods to textualist criteria (at cognitive and institutional cost) or operate outside the Hanbali framework entirely, forfeiting institutional recognition.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, rationalist_jurists, payer,
    powerful, generational, constrained, continental).

% Pay through the constraint's exclusion of 'amal (living legal practice) and customary adaptation as valid sources. Local juridical traditions, regional adjustments, and pragmatic accommodations cannot be justified within the Hanbali framework unless they happen to align with explicit text or unanimous Companion opinion. Their practices are either canonized through fiat consensus or delegitimized as bid'ah.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, customary_practice_communities, payer,
    organized, biographical, constrained, regional).

% Would argue that qiyas and istihsan are not innovations but necessary extensions of divine intent to unprecedented cases; that reason, disciplined by Qur'an and Hadith, is a legitimate jurisprudential tool. They are structurally excluded from the Hanbali consensus mechanism because their methodological premises contradict the constraint's core gate.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, hanafi_school_jurists, excluded,
    institutional, generational, identity_locked, continental).

% Would argue for a hierarchical integration of sources (Qur'an > Hadith > Ijma > Qiyas) that permits qiyas as the fourth tier when earlier sources run out. Their exclusion from Hanbali consensus reflects disagreement on the legitimacy and role of analogical reasoning, not on the divine text's authority.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, shafii_school_jurists, excluded,
    institutional, generational, identity_locked, continental).

% Would argue that Medinan practice ('amal ahl al-Madina) is a valid source coordinate with text because the Prophet's community preserved his actual jurisprudential behavior. They are excluded because the constraint rejects community practice as a source unless it achieves unanimous Companion consensus — a gate that delegitimizes the Maliki appeal to living tradition.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, maliki_school_jurists, excluded,
    institutional, generational, identity_locked, continental).

% Study the constraint's structure, its operation across different Islamic societies and historical periods, and its interaction with competing methodological schools. They do not enforce the constraint but document how it shapes jurisprudential authority structures, epistemological hierarchies, and the fate of novel legal problems.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, islamic_legal_theorists, observer,
    analytical, generational, analytical, continental).

% Adopt and enforce the constraint instrumentally, using the textualist method's claim to immutability as a source of legitimacy. They benefit from the constraint's appearance of law-outside-politics — the constraint frames the ruler as bound to unambiguous text rather than exercising discretion. They administer the constraint selectively, applying strict textualism to subjects they wish to control and permitting looser reasoning where political interests favor it.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, political_authorities_claiming_hanbali_purity, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__hanbali_reading, political_authorities_claiming_hanbali_purity, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jurisprudential_method_kernel__hanbali_reading, hanbali_textualist_scholars).
narrative_ontology:fixing_cost_class(jurisprudential_method_kernel__hanbali_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a common hermeneutic method for Islamic jurisprudence: how to derive law from the textual sources (Qur'an and Hadith) in a principled way that all jurists can, in principle, verify. The constraint coordinates by fixing the rules for what counts as a valid legal argument, preventing ad-hoc reasoning and enabling scholarly debate to proceed from shared axioms.
% TRANSFER_FUNCTION: Transfers epistemological and institutional authority from rationalist jurists and adaptive customary-practice communities toward textualist scholars and hadith authenticators. The flow is not material wealth but interpretive power: the right to say what the law is, to determine consensus, to set the methodological boundaries of legitimate argument. Rationalist jurists lose the ability to extend the law through qiyas; communities lose the ability to justify practice through living tradition.
% ABSENT_VOICES: The Hanafi, Maliki, and Shafi'i schools are excluded from the Hanbali consensus mechanism by their methodological disagreements. So too are Islamic jurists who developed novel reasoning methods after the early period. Sufi jurisprudence, which sometimes integrated mystical insight, is structurally outside the framework. Regional legal practitioners who adapted law to local conditions without explicit Companion backing are absent from the legitimacy conversation entirely.
% DISAPPEARANCE_RATIONALE: Hanbali textualists argue that if the constraint disappeared, jurisprudence would collapse into arbitrary reasoning and bid'ah would corrupt the divine law — the world would rearrange toward chaos and innovation. Rationalist jurists and practitioners argue that if the constraint vanished, jurisprudence would adapt more fluidly to changing circumstances and legal schools would converge on methods that balance text with reason — the world would rearrange toward a more coherent jurisprudential ecology. The empirical question (what actually happened in Islamic legal history when this constraint was relaxed or tightened) is contested across the schools.
% FOUNDING_PROBLEM: Early Islamic jurisprudence showed inconsistency: different jurists extended the divine law to novel cases using different methods, producing conflicting rulings. The Hanbali textualist method was developed to eliminate this inconsistency by tying all jurisprudence to fixed textual sources and excluding discretionary reasoning (qiyas, istihsan) that could produce arbitrary outcomes. The method claims to be the restoration of the Prophet's own jurisprudence, before rationalist elaboration corrupted it.
% FOUNDING_PROBLEM_CORROBORATION: Hanbali textualists and traditionalist hadith scholars attest that the founding problem (inconsistency and innovation-drift) is still live and that strict textualism is the remedy. Rationalist jurists and comparative legal historians attest that the founding problem was real in the 2nd-3rd Islamic centuries but has been substantially resolved through 1,200+ years of jurisprudential refinement; that the Hanbali method, by excluding adaptive reasoning, creates NEW problems (inability to address unprecedented cases) while solving OLD ones. Non-Islamic and secular legal historians provide corroboration that rigid literalism constrains legal development, though this corroboration comes from outside the Islamic framework and is therefore rejected by textualists as illegitimate witness.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__hanbali_reading, contested).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__hanbali_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__hanbali_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(jurisprudential_method_kernel__hanbali_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__hanbali_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jurisprudential_method_kernel__hanbali_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jurisprudential_method_kernel__hanbali_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness begins low (0.31 at t0) because the constraint is newly articulated by Ahmad ibn Hanbal and represents a genuine methodological recovery effort against perceived innovation drift. By t=200 (8th century CE equivalent), extractiveness rises to 0.45 as the Hanbali school institutionalizes the method and begins suppressing qiyas-based reasoning in jurisprudential circles. The trajectory continues upward, reaching 0.68 by t=1200, as the constraint's enforcement apparatus hardens: textualist scholars gain more institutional power, suppress rationalist arguments more aggressively, and theater increases (the claim to pure textualism is invoked while actual practice becomes more selective and politically influenced). Theater_ratio rises from 0.08 to 0.41, indicating that an increasingly large share of the constraint's maintenance is performative: scholars cite the principle of textual purity while silently employing qiyas-like reasoning for cases the strict method cannot handle, or while political authorities invoke textualism selectively. Suppression_requirement parallels extractiveness: it starts at 0.52 (early suppression of rationalist methods is needed but not yet institutionalized) and rises to 0.72 (by the medieval period, active institutional machinery is required to keep rationalist jurisprudence from reasserting itself and to maintain the fiction of pure textualism when practice has become more hybrid). All measurements on a shared time grid: every metric is authored at t={0, 200, 400, 600, 900, 1200}.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (Hanbali scholars) and the rationalist jurist seat (constrained payers) experience the constraint as fundamentally different types. From the textualist seat: this is coordination, restoring law to divine will, with incidental costs to those who preferred reasoning methods. From the rationalist seat: this is enforced suppression of legitimate methodology, with a cover story of textual purity. The directionality reversal (d near 0.05 vs d near 0.85) predicts this divergence. A political authority using the constraint instrumentally sits near d=0.45, neither fully benefiting nor fully paying.
 *
 * DIRECTIONALITY LOGIC:
 *   Hanbali textualist scholars are the primary beneficiaries (declared in beneficiaries[]) and agenda-setters (role: agenda_setter). They have institutional power, high time horizon, and identity-locked exit — their self-concept is constituted through doctrinal purity. Purist hadith transmitters are secondary beneficiaries: they collect authority and resources from the constraint's elevation of hadith authentication. Rationalist jurists are the primary victims: they cannot use qiyas, cannot extend the law through reason, are excluded from consensus mechanisms, and face institutional suppression. Customary practice communities are secondary victims: their living practices are delegitimized. Political authorities are ambiguous: they benefit from the legitimacy claim but face constraints when they need jurisprudential flexibility. The engine derives directionality from these beneficiary/victim declarations and exit options; no override needed for the core seats, though the political authority's dual positioning (beneficiary + payer) warrants close attention to how the engine handles secondary_role interactions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (inconsistency in early jurisprudence across different reasoning methods) was genuine at t=0. By t=400-600, the problem was substantially solved: Islamic jurisprudence had stabilized into coherent schools, and the diversity of methods had produced a mature legal tradition rather than chaos. Yet the textualist constraint persists and even intensifies through t=1200. This is classic mandatrophy: the constraint's founding mandate has outlived its function. The theater_ratio rise (0.08 → 0.41) supports this: scholars increasingly invoke textualism rhetorically while silently using rational methods, creating a gap between stated method and actual practice. The constraint should be classified as piton in the later period (t=900-1200) when the mandate is dead but the institutional apparatus maintains itself theatrically. The tangled_rope claim at present (t=1200) reflects the constraint's current ambiguous state: it still exerts extractive pressure (high ε), still requires enforcement, still has nominal beneficiaries and victims, but the coordination function it once served is atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    methodological_purity_vs_pragmatic_practice,
    'Does the Hanbali textualist method operate as stated in jurisprudential theory, or do practitioners silently employ qiyas-like reasoning for cases the strict method cannot handle?',
    'Detailed historical-textual analysis of Hanbali juridical decisions comparing stated method to actual argumentation patterns; ethnographic study of contemporary Hanbali legal interpretation in jurisdictions where it is applied.',
    'If the textualist method operates as stated, the constraint''s extractiveness and suppression metrics are accurate and the classification stands. If practitioners routinely use qiyas while denying it, the theater_ratio is understated and the constraint should be reclassified as piton (performance with atrophied function).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(methodological_purity_vs_pragmatic_practice, empirical, 'Whether stated method matches actual jurisprudential practice.').

omega_variable(
    founding_mandate_obsolescence,
    'Is the textualist constraint''s founding mandate (preventing arbitrary reasoning and jurisprudential drift) still live, or has Islamic jurisprudential tradition matured to the point where mature schools can maintain coherence without strict textualism?',
    'Comparative legal-historical analysis: does jurisprudential diversity increase or decrease when the constraint is relaxed? Do mature legal traditions with qiyas maintain coherence or collapse? What evidence do jurists themselves cite for the ongoing necessity of textualism?',
    'If the mandate is dead and the constraint persists, the constraint is mandatrophy-classifiable and should be reclassified from tangled_rope to piton. If the mandate is still live, the constraint retains its functional justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_mandate_obsolescence, empirical, 'Whether the constraint''s founding problem is still live or has been resolved.').

omega_variable(
    literal_text_interpretation_ambiguity,
    'Does ''literal text'' have a determinate meaning independent of interpretive method, or does the apparently literal method itself encode substantive interpretive choices that differ from other schools'' choices?',
    'Textual comparison of how Hanbali, Hanafi, and Shafi''i scholars interpret the same Qur''anic verses and Hadith reports: do they arrive at different meanings despite all claiming to follow the literal text?',
    'If literal meaning is determinate, the textualist claim to methodological purity is defensible. If all schools claim to follow literal text but arrive at different conclusions, the Hanbali constraint''s claim to escape interpretation is false — it merely hides its interpretive moves rather than avoiding them. This affects the constraint''s natural-law claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(literal_text_interpretation_ambiguity, conceptual, 'Whether literal text interpretation is method-independent or method-laden.').

omega_variable(
    consensus_determination_authority,
    'Who determines what counts as valid ijma (unanimous Companion consensus)? Is this determination itself part of the constraint (encoded in Hanbali methodology) or external to it?',
    'Historical analysis of how different Hanbali scholars determined and contested consensus claims; examination of cases where putative consensus was later disputed.',
    'If consensus determination is internal to the constraint (Hanbali scholars decide what counts as consensus), then the constraint has a hidden degrees-of-freedom that undermines its claim to textual determinacy. If external, the constraint is more narrowly specified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consensus_determination_authority, conceptual, 'Whether consensus determination is method-encoded or externally adjudicated.').

omega_variable(
    political_authority_instrumentalization,
    'Do political authorities that adopt the textualist constraint apply it uniformly, or do they invoke textualism selectively for cases that serve their interests while employing looser reasoning elsewhere?',
    'Historical case study of jurisdictions under Hanbali-dominant rule: compare the jurisprudential method used for cases affecting state power (taxation, punishment, succession) versus cases affecting subjects without state interest.',
    'If application is uniform, the constraint''s extractiveness is accurately measured. If selective, the beneficiary/victim structure is more complex — the state becomes a major secondary beneficiary, and the constraint operates partly as a tool of political control (raising extractiveness and mandatrophy questions).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(political_authority_instrumentalization, empirical, 'Whether textualist constraint application is uniform or politically selective.').

omega_variable(
    bid_ah_definition_stability,
    'Is ''bid''ah'' (innovation) a stable category, or does its meaning shift depending on which innovations threaten the textualist authority structure at a given time?',
    'Genealogical analysis of which practices Hanbali scholars have labeled bid''ah across time: are the labeled innovations consistent with a principled criterion, or do they target whatever threatens textualist dominance?',
    'If bid''ah is a stable category defined by opposition to Qur''an/Hadith/consensus, the constraint is coherent. If the category shifts to protect textualist authority, the constraint is partially performative and extractive rather than coordinative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bid_ah_definition_stability, empirical, 'Whether bid''ah classification is principled or instrumental.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__hanbali_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t0, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(juri_tr_t0, projected).
narrative_ontology:measurement(juri_tr_t200, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 200, 0.15).
narrative_ontology:measurement_basis(juri_tr_t200, observed).
narrative_ontology:measurement(juri_tr_t400, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 400, 0.25).
narrative_ontology:measurement_basis(juri_tr_t400, observed).
narrative_ontology:measurement(juri_tr_t600, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 600, 0.35).
narrative_ontology:measurement_basis(juri_tr_t600, observed).
narrative_ontology:measurement(juri_tr_t900, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 900, 0.39).
narrative_ontology:measurement_basis(juri_tr_t900, observed).
narrative_ontology:measurement(juri_tr_t1200, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 1200, 0.41).
narrative_ontology:measurement_basis(juri_tr_t1200, observed).

% Extraction over time
narrative_ontology:measurement(juri_be_t0, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 0, 0.31).
narrative_ontology:measurement_basis(juri_be_t0, projected).
narrative_ontology:measurement(juri_be_t200, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 200, 0.45).
narrative_ontology:measurement_basis(juri_be_t200, observed).
narrative_ontology:measurement(juri_be_t400, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 400, 0.58).
narrative_ontology:measurement_basis(juri_be_t400, observed).
narrative_ontology:measurement(juri_be_t600, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 600, 0.64).
narrative_ontology:measurement_basis(juri_be_t600, observed).
narrative_ontology:measurement(juri_be_t900, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 900, 0.66).
narrative_ontology:measurement_basis(juri_be_t900, observed).
narrative_ontology:measurement(juri_be_t1200, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 1200, 0.68).
narrative_ontology:measurement_basis(juri_be_t1200, observed).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t0, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(juri_su_t0, projected).
narrative_ontology:measurement(juri_su_t200, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 200, 0.61).
narrative_ontology:measurement_basis(juri_su_t200, observed).
narrative_ontology:measurement(juri_su_t400, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 400, 0.67).
narrative_ontology:measurement_basis(juri_su_t400, observed).
narrative_ontology:measurement(juri_su_t600, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 600, 0.7).
narrative_ontology:measurement_basis(juri_su_t600, observed).
narrative_ontology:measurement(juri_su_t900, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 900, 0.71).
narrative_ontology:measurement_basis(juri_su_t900, observed).
narrative_ontology:measurement(juri_su_t1200, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 1200, 0.72).
narrative_ontology:measurement_basis(juri_su_t1200, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__hanbali_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jurisprudential_method_kernel__hanbali_reading, 0.12).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel__hanafi_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel__maliki_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel__shafii_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel jurisprudential_method_kernel, which encodes the fundamental question of how Islamic law derives from authoritative sources. The Hanbali reading rejects analogical reasoning (qiyas) and juristic preference (istihsan) as bid'ah. The Hanafi reading affirms both as legitimate tools. The Maliki reading grounds law in Medinan living practice. The Shafi'i reading hierarchizes sources. These are structurally distinct constraints with different ε values (Hanbali: high ε on qiyas rejection; Hanafi: low ε on qiyas acceptance; Shafi'i: medium ε via hierarchization). The network links all siblings; the kernel context documents which readings foreclose which others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jurisprudential_method_kernel__hanbali_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
