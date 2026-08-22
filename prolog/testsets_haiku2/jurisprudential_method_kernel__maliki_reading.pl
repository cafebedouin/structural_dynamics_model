% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__maliki_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__maliki_reading, []).

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
 *   constraint_id: jurisprudential_method_kernel__maliki_reading
 *   human_readable: Maliki Jurisprudential Method: Qur'an/Hadith as Practiced in Medina
 *   domain: religious/legal philosophy/institutional history
 *
 * SUMMARY:
 *   The Maliki jurisprudential reading instantiates a method for Islamic law
 *   in which the living tradition of the Medinan community ('amal ahl
 *   al-Madina)—the authenticated practices of early Medina—serves as a valid
 *   and privileged source of law alongside the Qur'an and Hadith. This
 *   reading claims that Medina preserved the Prophet's practice most
 *   faithfully because he lived and taught there. The reading contrasts with
 *   the Hanafi privileging of reason and analogical extension (qiyas), the
 *   Hanbali rejection of 'amal as innovation, and the Shafi'i subordination
 *   of all methods to a strict four-tier hierarchy topped by hadith
 *   transmission. As a kernel reading, this constraint describes the Maliki
 *   understanding of what counts as valid law, not the other schools'
 *   understandings—the sibling readings are different constraints. The
 *   extraction measured here is the epistemological authority transferred
 *   from all non-Medinan interpretive claims to the Medinan scholarly
 *   lineage; the coordination function is the resolution of indeterminacy
 *   when texts do not directly address a case.
 *
 * KEY AGENTS:
 *   - Medinan scholarly lineage: institutional agenda-setter and primary beneficiary; controls the interpretation of authentic 'amal and claims historical proximity to the Prophet's practice
 *   - Non-Medinan schools (Hanafi, Shafi'i, Hanbali): powerful payers; must defend their methodologies against the claim that Medinan practice is epistemologically privileged
 *   - Muftis and judicial authorities in Maliki jurisdictions: institutional agenda-setters; enforce Maliki methodology and maintain the constraint through adjudication
 *   - Transmission scholars (hadith experts): organized beneficiaries; gain prestige and institutional demand from the requirement to authenticate Medinan practice traditions
 *   - Non-Maliki schools' scholars in other regions: powerful but constrained payers; cannot adjudicate within Maliki jurisdictions despite having substantial scholarly authority elsewhere
 *   - Common believers in Maliki jurisdictions: powerless beneficiaries; follow the law as adjudicated but cannot contest the methodology
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__maliki_reading, 0.58).
domain_priors:suppression_score(jurisprudential_method_kernel__maliki_reading, 0.42).
domain_priors:theater_ratio(jurisprudential_method_kernel__maliki_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__maliki_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__maliki_reading, "Maliki Jurisprudential Method: Qur'an/Hadith as Practiced in Medina").
narrative_ontology:topic_domain(jurisprudential_method_kernel__maliki_reading, "religious/legal philosophy/institutional history").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__maliki_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__maliki_reading, 'c918c7d3-822a-4832-9322-63a0d9f62513').
narrative_ontology:cs_kernel_codification('c918c7d3-822a-4832-9322-63a0d9f62513', formalized).
narrative_ontology:cs_authority_grounding('c918c7d3-822a-4832-9322-63a0d9f62513', lineage).
narrative_ontology:cs_interpretation_layer_present('c918c7d3-822a-4832-9322-63a0d9f62513').
narrative_ontology:cs_reading_relation('c918c7d3-822a-4832-9322-63a0d9f62513', jurisprudential_method_kernel__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('c918c7d3-822a-4832-9322-63a0d9f62513', jurisprudential_method_kernel__shafii_reading, coexists_with).
narrative_ontology:cs_reading_relation('c918c7d3-822a-4832-9322-63a0d9f62513', jurisprudential_method_kernel__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('c918c7d3-822a-4832-9322-63a0d9f62513', foundational, medinan_practice_preserves_prophetic_fidelity).
narrative_ontology:cs_axiom_status(medinan_practice_preserves_prophetic_fidelity, holdable).
narrative_ontology:cs_axiom_grounding('c918c7d3-822a-4832-9322-63a0d9f62513', medinan_practice_preserves_prophetic_fidelity, empirically_contingent).
narrative_ontology:cs_axiom('c918c7d3-822a-4832-9322-63a0d9f62513', secondary, living_tradition_transmits_tacit_wisdom).
narrative_ontology:cs_axiom_status(living_tradition_transmits_tacit_wisdom, holdable).
narrative_ontology:cs_axiom_grounding('c918c7d3-822a-4832-9322-63a0d9f62513', living_tradition_transmits_tacit_wisdom, deontological).
narrative_ontology:cs_reference_frame('c918c7d3-822a-4832-9322-63a0d9f62513', medinan_prophetic_practice_preservation).
narrative_ontology:cs_drift_state('c918c7d3-822a-4832-9322-63a0d9f62513', contemporary_historical_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c918c7d3-822a-4832-9322-63a0d9f62513', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__maliki_reading, medinan_scholarly_lineage).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__maliki_reading, non_medinan_interpretive_claims).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__maliki_reading, medinan_jurists_current_generation).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__maliki_reading, transmission_scholars_hadith_experts).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__maliki_reading, common_believers_and_laypeople).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__maliki_reading, non_maliki_schools_scholars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Transmits and adjudicates the living tradition ('amal ahl al-Madina) as a valid jurisprudential source. Controls the interpretation of what counts as authenticated practice from the Prophet's era. Claims authority through historical proximity to the Prophet's actual conduct in Medina. Benefits from the epistemological privilege: their readings are treated as closer to divine intent than reasoning from isolated texts.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, medinan_scholarly_lineage, agenda_setter,
    institutional, civilizational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__maliki_reading, medinan_scholarly_lineage, beneficiary).

% Non-Medinan legal interpretations (Hanafi, Shafi'i, Hanbali schools) must defend their methodologies against the Maliki privileging of Medinan practice. They bear the cost of having to demonstrate that their methods (qiyas, istihsan, methodological standardization, literal textuality) are not innovations corrupting the kernel. Their interpretive authority is structurally subordinated to Medinan lineage claims.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, non_medinan_interpretive_claims, payer,
    powerful, civilizational, constrained, global).

% Contemporary scholars working in the Maliki tradition inherit the methodological authority of Medinan proximity. They do not personally authenticate the traditions but operate within a framework that grants their interpretations presumptive validity based on lineage continuity. Exit would require abandoning professional identity and scholarly position within the tradition.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, medinan_jurists_current_generation, beneficiary,
    organized, biographical, identity_locked, regional).

% Magistrates and legal authorities in Maliki jurisdictions (historically: Andalusia, North Africa, West Africa) enforce legal judgments grounded in Maliki methodology. They maintain the constraint by adjudicating disputes according to the hierarchy that privileges Medinan practice. Their legitimacy rests on the kernel's authority.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, muftis_and_judicial_authorities, agenda_setter,
    institutional, generational, constrained, regional).

% Hanafi, Shafi'i, and Hanbali jurists are excluded from adjudicating within Maliki jurisdictions, even where their schools are numerically dominant elsewhere. They are constrained from claiming equal interpretive authority within Maliki-governed regions. Their objections to the 'amal epistemology are structural suppressions—they are not seated in the interpretive council where Maliki methodology is adjudicated.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, non_maliki_schools_scholars, payer,
    powerful, civilizational, constrained, global).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__maliki_reading, non_maliki_schools_scholars, excluded).

% Scholars specializing in hadith authentication and isnad criticism gain institutional prestige from the requirement to validate practice traditions. The Maliki method creates demand for expertise in authenticating Medinan chains of transmission. They benefit from the methodological privilege granted to traditional attestation.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, transmission_scholars_hadith_experts, beneficiary,
    organized, generational, constrained, global).

% In Maliki jurisdictions, believers follow the law as adjudicated by Maliki authorities. They benefit from a coherent, historically-grounded legal framework that claims fidelity to the Prophet's own practice. They bear the cost of exclusion from alternative legal methodologies should they migrate or face jurisdictional change. Their exit is religious identity and geographical relocation.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, common_believers_and_laypeople, beneficiary,
    powerless, biographical, trapped, regional).

% Reform movements, rationalist theologians, and modernists who argue for direct textual reasoning or rejection of 'amal as binding are excluded from authority within traditional Maliki frameworks. They are structurally suppressed by the epistemological privilege granted to lineage-authenticated practice. Their exit is doctrinal: adopt a different school or theological tradition.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, theological_opponents_and_innovators, excluded,
    moderate, biographical, mobile, global).

% External scholars and comparative jurisprudents document how the Maliki method operates and compare it to rival methodologies. They hold no stake in the constraint's operation but provide epistemic service in analyzing the kernel and its readings.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, comparative_legal_analysis, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jurisprudential_method_kernel__maliki_reading, medinan_scholarly_lineage).
narrative_ontology:fixing_cost_class(jurisprudential_method_kernel__maliki_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the problem of how to apply divinely-revealed law to new cases and diverse contexts: by privileging the living tradition of the Medinan community as the most faithful preservation of the Prophet's actual practice, the method provides a stable hermeneutic anchor that prevents unlimited proliferation of interpretive schools and maintains continuity with the revelation.
% TRANSFER_FUNCTION: Transfers epistemological authority (the power to say what the law is) from any jurist capable of reasoning about texts to the Medinan scholarly lineage whose position in historical transmission grants them privileged access to divine intent. Non-Medinan interpretations must defend themselves as secondary to this core tradition.
% ABSENT_VOICES: Hanafi, Shafi'i, and Hanbali scholars whose methodologies operate in non-Maliki jurisdictions would argue that reason ('aql), methodological consistency, and literal textuality are equal or superior grounds for interpretation. Non-elite lay commentators on the law are not seated in the adjudication council; they follow rather than contest.
% DISAPPEARANCE_RATIONALE: From the Maliki perspective: if the constraint vanished, legal interpretation would fragment into competing methodologies with no shared anchor, leading to jurisdictional chaos and loss of fidelity to the Prophet's practice. From non-Maliki schools: if it vanished, legal reasoning could proceed without epistemological subordination to one region's historical claims, allowing methodological equality and potentially better adaptation to novel contexts.
% FOUNDING_PROBLEM: After the Prophet Muhammad's death, the Islamic community faced the problem of deriving law for cases not explicitly addressed in revelation or Hadith. How could the community remain faithful to divine intent while adapting to new circumstances? The Maliki reading claims that the Medinan community's lived practice (the 'amal) is the most faithful solution because Medina was the Prophet's seat and preserved his actual conduct most completely.
% FOUNDING_PROBLEM_CORROBORATION: Maliki scholars and North African Maliki authorities attest the problem is live and that 'amal is the solution. Non-Maliki schools (Hanafi, Shafi'i, Hanbali) attest the founding problem is real but argue their methodologies solve it better—qiyas and istihsan for Hanafis, methodological hierarchy for Shafi'is, literal textuality for Hanbalis. Modern legal historians (Hallaq, Lowry) note the founding problem was indeed live but document that the schools' claimed solutions were constructed *after* the Prophet's era, not derived from his direct practice. The corroboration is mixed and disciplinarily divided.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__maliki_reading, contested).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__maliki_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__maliki_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jurisprudential_method_kernel__maliki_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__maliki_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__maliki_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jurisprudential_method_kernel__maliki_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jurisprudential_method_kernel__maliki_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58 at interval end) because the constraint transfers substantial epistemological authority to the Medinan lineage but is not pure extraction—it solves a real coordination problem (how to apply law to novel cases) and the non-Maliki schools retain authority in their own regions and can defend their methodologies as coherent alternatives. Suppression is moderate (0.42) because the constraint operates through epistemological privilege rather than direct coercion; non-Maliki schools can articulate their objections and maintain institutional presence globally, but are excluded from adjudication within Maliki jurisdictions. Theater ratio is moderate-low (0.28) because the constraint's functional core (determining law from traditional practice) is authentic, though performative elements increase over time as the lineage invests in theatrical validation of continuity. Accessibility collapse is moderate-high (0.65): once the 'amal epistemology is understood, alternatives become structurally excluded within the framework, yet not completely—other schools remain live positions for those who exit to different jurisdictions or adopt different methodologies. Resistance is moderate (0.55): non-Maliki schools actively mount resistance through defending their own methods as superior; common believers bear the constraint quietly. The time series tracks extractiveness rising from early consolidation (0.35) through the classical period of Maliki expansion (0.62 at t=1100) and then slightly declining (0.58 at t=1400) as colonial and modern pressures began fragmenting the Maliki monopoly. Theater and suppression requirement track similarly, both rising slightly through the classical period as the lineage invested more in theatrical performance of continuity, then stabilizing as external challenge became structural.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between Maliki scholars and non-Maliki schools is structural. From the Maliki seat, the constraint is a genuine coordination mechanism grounded in historical fidelity—'amal preserves the Prophet's practice, and this preservation solves the problem of indeterminacy. From the Hanafi seat, the constraint is pure extraction masquerading as history—it privileges one community's claims of proximity at the expense of reason and methodological consistency. From the Shafi'i seat, the constraint is incoherent—a strict hierarchy based on text transmission (Qur'an > Hadith > Ijma > Qiyas) is superior to community practice, which is ambiguous and contestable. From the Hanbali seat, the constraint is bid'ah (innovation)—the Prophet's practice is available directly in Hadith without intermediate 'amal construction. The engine computes these divergent perceptions from the structural data: Maliki scholars' identity-locked position in the lineage produces one directionality; non-Maliki scholars' excluded but powerful position produces another; the asymmetry emerges from the stakeholder matrix, not from observer preference.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is asymmetric between seats. The Medinan scholarly lineage is the structural beneficiary—their interpretations are treated as presumptively authoritative by virtue of lineage, not by virtue of reasoned argument (d≈0.1, near beneficiary end). Non-Medinan interpretive claims are the structural target—they must defend themselves as secondary to Medinan practice, bearing the burden of proof (d≈0.9, near target end). Muftis and judicial authorities sit at d≈0.3 (constrained beneficiaries: they enforce the method but inherit its authority rather than generating it independently). Transmission scholars sit at d≈0.2 (beneficiaries: they gain prestige from the requirement to authenticate traditions). Common believers sit at d≈0.5 (symmetric: they benefit from a coherent legal framework but bear the cost of exclusion from alternative methodologies—their exit is religious and geographic identity change). The constraint's directionality drives asymmetric extraction: the Medinan lineage collects epistemological rents; non-Maliki schools pay the cost of subordination; the rest of the architecture (muftis, believers) sits between. This asymmetry explains why the constraint is Tangled Rope rather than Rope: it coordinates (solves the problem of applying revelation to novel cases) AND extracts (privileges one lineage over all others) AND requires active enforcement (adjudication, exclusion from judicial authority, suppression of alternative methodologies within Maliki jurisdictions).
 *
 * MANDATROPHY ANALYSIS:
 *   The Maliki reading faces a mandatrophy risk: the founding problem (how to apply revelation to new cases without fragmenting) has been addressed differently by four competing schools, all of which claim fidelity to the Prophet. The claim that Medina uniquely preserves the Prophet's practice is contested by non-Maliki schools who argue their methodologies are equally or more faithful. Modern legal historians (Hallaq, Lowry) show that none of the schools' methodologies are direct applications of the Prophet's actual practice—all four readings are post-hoc constructed frameworks. If this historical fact becomes widely accepted, the Maliki reading's foundational axiom (medinan_practice_preserves_prophetic_fidelity) loses its empirical ground, though not its deontological authority (the commitment to privilege Medina as a matter of tradition). The constraint avoids mandatrophy reclassification as long as the Maliki scholarly lineage can maintain the axiom's status as 'holdable' in faith-based terms despite empirical contestation. The measurement series shows extractiveness rising through the classical period (when the Maliki system was administratively dominant and could suppress alternative methodologies) and then stabilizing (when colonial administrative systems undermined Maliki judicial monopoly, reducing the constraint's effective enforcement). The theater ratio's rise suggests increasing performative validation of continuity claims as historical pressure mounted—a sign of identity-locked defense rather than genuine functional deepening.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    medinan_practice_authenticity,
    'Did the Medinan community actually preserve the Prophet''s practice more faithfully than other communities, or is this a post-hoc constructed claim by Maliki scholars to ground their authority?',
    'Comparative historical analysis of early Islamic practice across regions (Medina, Mecca, Kufa, Basra, Egypt). Cross-examination of hadith chains for regional variation and attestation patterns. Legal-historical scholarship (Hallaq, Lowry, Melchert) documents this ambiguity; no empirical resolution is foreseeable because practice reconstruction depends on the same disputed sources.',
    'If Medinan practice is shown to be no more authentic than other regions'' practices, the reading''s foundational axiom (medinan_practice_preserves_prophetic_fidelity) becomes historical myth rather than empirical claim. The reading could persist on deontological grounds (the commitment to privilege Medina as tradition) but loses its empirical legitimacy. This could trigger mandate-atrophy analysis: the constraint''s authority rests on a falsified empirical premise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(medinan_practice_authenticity, empirical, 'Whether Medina uniquely preserved the Prophet''s practice or this is mythologized.').

omega_variable(
    coordination_vs_extraction_boundary,
    'Is the transfer of epistemological authority to Medinan lineage necessary for solving the coordination problem (applying revelation to novel cases), or is it an extractive add-on that rides on genuine coordination?',
    'Examine whether the Hanafi, Shafi''i, and Hanbali methods solve the coordination problem equally well using different epistemological bases. If all four schools produce stable, coherent legal systems addressing the same cases, then the Medinan privilege is extractive, not coordinative.',
    'If the coordination problem can be solved without Medinan privilege, the constraint should be reclassified from Tangled Rope (coordination + extraction) to Snare (pure extraction masquerading as coordination). The constraint''s claimed justification would be revealed as cover story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, conceptual, 'Whether epistemological privilege is necessary for coordination or merely extractive.').

omega_variable(
    lineage_identity_lock_mechanism,
    'Is the identity-lock of Medinan scholars and Maliki jurists structural (they are professionally constituted by transmission of ''amal) or ideological (they have chosen to fuse their identity with the tradition)?',
    'Post-exit analysis: when scholars or jurists exit the Maliki tradition (e.g., convert to Hanafi or modernist frameworks), does their identity-lock persist, diminish, or dissolve? If identity persists after exit, the lock is partially internalized (ideological). If it dissolves, the lock is structural.',
    'If the lock is largely structural, exit costs for Medinan scholars are reduced by institutional change (e.g., non-Maliki jurisdictions adopting Maliki authority). If largely ideological, the constraint persists through internalized commitment even as external institutional pressure weakens. This affects suppression_requirement and theater_ratio trajectories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lineage_identity_lock_mechanism, empirical, 'Whether scholarly identity-lock to tradition is structural or ideological.').

omega_variable(
    other_schools_as_victims_or_competitors,
    'Are Hanafi, Shafi''i, and Hanbali schools positioned as structural victims (excluded from Maliki jurisdictions, bearing extraction costs) or as institutional competitors (retaining authority in their own regions, capable of defending their methodologies)?',
    'Historical examination of jurisdiction by school: in regions where Maliki law was imposed (North Africa, parts of Andalusia), did non-Maliki schools lose institutional authority? In regions where other schools were dominant (Hanafi: Ottoman, Central Asia; Shafi''i: Egypt, Southeast Asia), could they resist Maliki claims? Geographic analysis of victimhood vs. competition.',
    'If non-Maliki schools are primarily victims (excluded, subordinated), the constraint''s suppression is higher and extraction is asymmetric. If primarily competitors (retaining regional authority), suppression is lower and extraction is mutual within the Islamic legal world. This affects classification: pure extraction (snare) vs. competitive extraction (tangled rope with lower suppression).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(other_schools_as_victims_or_competitors, empirical, 'Whether non-Maliki schools are structural victims or institutional competitors.').

omega_variable(
    reform_and_modernist_pressure,
    'As modern administrative and legal systems have displaced Islamic jurisprudential authority (colonial law, nation-states), have reform and modernist critiques of ''amal-based reasoning fundamentally challenged the Maliki reading, or has the reading adapted to maintain its authority?',
    'Examination of modern Maliki jurisprudential writing (19th–21st centuries): do contemporary Maliki scholars defend ''amal using new rationales (e.g., custom-based positive law), or do they cling to the original epistemological claim? If adaptation occurs, the reading has undergone axiom revision.',
    'If the reading has adapted axioms (reframing ''amal as customary law, cultural heritage), it has acknowledged the empirical challenge while maintaining institutional identity. This is axiom revision rather than foreclosure. If it clings to the original claim, it remains vulnerable to being reclassified as incoherent in modern legal discourse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_and_modernist_pressure, empirical, 'Whether Maliki reading adapts to modernization or clings to original claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__maliki_reading, 0, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t0, jurisprudential_method_kernel__maliki_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(juri_tr_t0, observed).
narrative_ontology:measurement(juri_tr_t200, jurisprudential_method_kernel__maliki_reading, theater_ratio, 200, 0.16).
narrative_ontology:measurement_basis(juri_tr_t200, observed).
narrative_ontology:measurement(juri_tr_t500, jurisprudential_method_kernel__maliki_reading, theater_ratio, 500, 0.22).
narrative_ontology:measurement_basis(juri_tr_t500, observed).
narrative_ontology:measurement(juri_tr_t800, jurisprudential_method_kernel__maliki_reading, theater_ratio, 800, 0.26).
narrative_ontology:measurement_basis(juri_tr_t800, observed).
narrative_ontology:measurement(juri_tr_t1100, jurisprudential_method_kernel__maliki_reading, theater_ratio, 1100, 0.29).
narrative_ontology:measurement_basis(juri_tr_t1100, observed).
narrative_ontology:measurement(juri_tr_t1400, jurisprudential_method_kernel__maliki_reading, theater_ratio, 1400, 0.28).
narrative_ontology:measurement_basis(juri_tr_t1400, observed).

% Extraction over time
narrative_ontology:measurement(juri_be_t0, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(juri_be_t0, observed).
narrative_ontology:measurement(juri_be_t200, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 200, 0.42).
narrative_ontology:measurement_basis(juri_be_t200, observed).
narrative_ontology:measurement(juri_be_t500, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 500, 0.52).
narrative_ontology:measurement_basis(juri_be_t500, observed).
narrative_ontology:measurement(juri_be_t800, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 800, 0.58).
narrative_ontology:measurement_basis(juri_be_t800, observed).
narrative_ontology:measurement(juri_be_t1100, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 1100, 0.62).
narrative_ontology:measurement_basis(juri_be_t1100, observed).
narrative_ontology:measurement(juri_be_t1400, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 1400, 0.58).
narrative_ontology:measurement_basis(juri_be_t1400, observed).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t0, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement_basis(juri_su_t0, observed).
narrative_ontology:measurement(juri_su_t200, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 200, 0.35).
narrative_ontology:measurement_basis(juri_su_t200, observed).
narrative_ontology:measurement(juri_su_t500, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 500, 0.4).
narrative_ontology:measurement_basis(juri_su_t500, observed).
narrative_ontology:measurement(juri_su_t800, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 800, 0.43).
narrative_ontology:measurement_basis(juri_su_t800, observed).
narrative_ontology:measurement(juri_su_t1100, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 1100, 0.44).
narrative_ontology:measurement_basis(juri_su_t1100, observed).
narrative_ontology:measurement(juri_su_t1400, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 1400, 0.42).
narrative_ontology:measurement_basis(juri_su_t1400, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__maliki_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jurisprudential_method_kernel__maliki_reading, 0.12).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel__hanafi_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel__shafii_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel__hanbali_reading).

% DUAL FORMULATION NOTE:
% The jurisprudential_method_kernel constraint family consists of four readings: Maliki, Hanafi, Shafi'i, and Hanbali. Each reading instantiates a different constraint because each answers the authorization question (what counts as valid reasoning for law) differently. The Maliki reading privileges Medinan practice ('amal ahl al-Madina); the Hanafi reading privileges reason and analogical extension (qiyas); the Shafi'i reading privileges a strict textual hierarchy; the Hanbali reading privileges literal text and Companion consensus. These are not four perspectives on one constraint—they are four distinct constraints grounded in different epistemological bases and producing different victim/beneficiary structures. All four readings remain live in Islamic jurisprudential discourse, each claimed by its institutional lineage as the truest interpretation of the Prophet's legacy. This story instantiates the Maliki reading only; sister stories instantiate the others. The affects_constraints edges link this reading to its siblings, indicating that changes in the empirical status of any reading (e.g., historical evidence undermining the Maliki claim to preserve the Prophet's practice) create downstream pressure on the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jurisprudential_method_kernel__maliki_reading, powerful, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
