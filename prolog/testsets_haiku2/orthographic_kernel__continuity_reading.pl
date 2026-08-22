% ============================================================================
% CONSTRAINT STORY: orthographic_kernel__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_kernel__continuity_reading, []).

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
 *   constraint_id: orthographic_kernel__continuity_reading
 *   human_readable: Arabic Script as Ottoman Cultural Continuity Anchor
 *   domain: political_linguistics/state_formation/commitment_systems
 *
 * SUMMARY:
 *   The Ottoman Empire committed to Arabic script as the orthography of state
 *   authority, governance, and Islamic learning, framing this choice as
 *   preserving continuity with Islamic tradition and Ottoman legitimacy. This
 *   constraint story instantiates the continuity_reading: the script choice
 *   is understood as a genuine coordination solution to the founding problem
 *   of maintaining Islamic textual unity and Ottoman cultural coherence.
 *   However, by the 17th–18th centuries, this commitment began extracting
 *   substantial opportunity costs: the state modernization apparatus was
 *   blocked from rapid orthographic modernization that would have accelerated
 *   military and technological adoption, and mass literacy remained
 *   impossible because the complexity of Arabic script made education
 *   expensive and slow. The continuity reading frames these costs as
 *   legitimate; the sibling modernization_reading frames them as hidden
 *   extraction, and the rupture_reading frames them as a system deliberately
 *   designed for cultural preservation that eventually had to be violently
 *   displaced. This story narrates the constraint from the continuity seat:
 *   preserving what makes it coherent as a reading, while the metrics measure
 *   the extraction dynamics the modernization and rupture readings
 *   foreground.
 *
 * KEY AGENTS:
 *   - Ottoman literate ulama: institutional authority rooted in Arabic-script textual mastery
 *   - Ottoman literate administrative class: career and identity locked to script, paying the continuity cost
 *   - Islamic institutional authority (global): benefits from unified orthographic system for textual transmission
 *   - Ottoman state modernization apparatus: blocked from fastest reform path, constrained to work within the continuity frame
 *   - Turkish reformers and modernizers: locked out of rapid implementation, eventually gain power and abandon the constraint entirely
 *   - Rural Ottoman population: excluded from literacy entirely, suffer highest barrier to education
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_kernel__continuity_reading, 0.68).
domain_priors:suppression_score(orthographic_kernel__continuity_reading, 0.72).
domain_priors:theater_ratio(orthographic_kernel__continuity_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_kernel__continuity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(orthographic_kernel__continuity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(orthographic_kernel__continuity_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_kernel__continuity_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(orthographic_kernel__continuity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_kernel__continuity_reading, tangled_rope).
narrative_ontology:human_readable(orthographic_kernel__continuity_reading, "Arabic Script as Ottoman Cultural Continuity Anchor").
narrative_ontology:topic_domain(orthographic_kernel__continuity_reading, "political_linguistics/state_formation/commitment_systems").

domain_priors:requires_active_enforcement(orthographic_kernel__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_kernel__continuity_reading, 'f173b767-6b2f-426e-bc43-499dffe9123a').
narrative_ontology:cs_kernel_codification('f173b767-6b2f-426e-bc43-499dffe9123a', fixed_text).
narrative_ontology:cs_authority_grounding('f173b767-6b2f-426e-bc43-499dffe9123a', lineage).
narrative_ontology:cs_interpretation_layer_present('f173b767-6b2f-426e-bc43-499dffe9123a').
narrative_ontology:cs_reading_relation('f173b767-6b2f-426e-bc43-499dffe9123a', orthographic_kernel__modernization_reading, coexists_with).
narrative_ontology:cs_reading_relation('f173b767-6b2f-426e-bc43-499dffe9123a', orthographic_kernel__rupture_reading, coexists_with).
narrative_ontology:cs_axiom('f173b767-6b2f-426e-bc43-499dffe9123a', foundational, islamic_textual_unity_preservable_via_script_continuity).
narrative_ontology:cs_axiom_status(islamic_textual_unity_preservable_via_script_continuity, holdable).
narrative_ontology:cs_axiom_grounding('f173b767-6b2f-426e-bc43-499dffe9123a', islamic_textual_unity_preservable_via_script_continuity, deontological).
narrative_ontology:cs_axiom('f173b767-6b2f-426e-bc43-499dffe9123a', foundational, ottoman_legitimacy_requires_islamic_continuity_chain).
narrative_ontology:cs_axiom_status(ottoman_legitimacy_requires_islamic_continuity_chain, holdable).
narrative_ontology:cs_axiom_grounding('f173b767-6b2f-426e-bc43-499dffe9123a', ottoman_legitimacy_requires_islamic_continuity_chain, conventional).
narrative_ontology:cs_axiom('f173b767-6b2f-426e-bc43-499dffe9123a', secondary, arabic_script_costs_justified_by_textual_preservation_benefit).
narrative_ontology:cs_axiom_status(arabic_script_costs_justified_by_textual_preservation_benefit, overridden).
narrative_ontology:cs_axiom_grounding('f173b767-6b2f-426e-bc43-499dffe9123a', arabic_script_costs_justified_by_textual_preservation_benefit, empirically_contingent).
narrative_ontology:cs_reference_frame('f173b767-6b2f-426e-bc43-499dffe9123a', ottoman_islamic_textual_authority_framework).
narrative_ontology:cs_drift_state('f173b767-6b2f-426e-bc43-499dffe9123a', late_ottoman_modernization_pressure_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f173b767-6b2f-426e-bc43-499dffe9123a', '').
narrative_ontology:cs_kernel_id(orthographic_kernel__continuity_reading, orthographic_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_kernel__continuity_reading, ottoman_literate_ulama).
narrative_ontology:constraint_beneficiary(orthographic_kernel__continuity_reading, islamic_institutional_authority).
narrative_ontology:constraint_victim(orthographic_kernel__continuity_reading, ottoman_literate_class).
narrative_ontology:constraint_victim(orthographic_kernel__continuity_reading, state_modernization_apparatus).
narrative_ontology:constraint_vindicates(orthographic_kernel__continuity_reading, islamic_textual_preservationism).
narrative_ontology:constraint_vindicates(orthographic_kernel__continuity_reading, ottoman_cultural_continuity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The scholarly and religious class whose authority and legitimacy are rooted in classical Islamic textual tradition written in Arabic script. Their interpretive lineage, jurisprudential authority, and social position depend on exclusive access to and mastery of Arabic-script texts. They set the terms of what constitutes legitimate Ottoman cultural knowledge and transmission.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, ottoman_literate_ulama, agenda_setter,
    institutional, civilizational, identity_locked, continental).

% Ottoman administrators, scholars, merchants, and educated elites who must maintain fluency in Arabic script to participate in formal governance, commerce, legal practice, and intellectual life. Their careers, status, and identity are fused with script mastery; switching to an alternative orthography would require learning and would disrupt the transmission of knowledge within their professional and familial lineages.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, ottoman_literate_class, payer,
    moderate, biographical, identity_locked, continental).

% The Ottoman and later Turkish state administrative machinery seeking to rationalize, standardize, and modernize governance, military capacity, and education. They are blocked from the most rapid orthographic modernization path (Latin-script transition) as long as the continuity reading holds institutional sway. The script constraint forces them to work within a system that privileges Islamic institutional authority over state modernization priorities.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, state_modernization_apparatus, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(orthographic_kernel__continuity_reading, state_modernization_apparatus, excluded).

% The broader Islamic textual and juridical tradition that benefits from Ottoman preservation of Arabic script as the medium of Islamic learning. This preserves the global circulation and authority of Islamic knowledge within a unified orthographic system, blocking fragmentation into competing script-based reading communities.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, islamic_institutional_authority, beneficiary,
    institutional, civilizational, arbitrage, global).

% Ottoman and Turkish reformers, modernizers, and nationalist intellectuals who would adopt Latin script (or other scripts) to accelerate scientific and technological adoption, simplify literacy campaigns, and align with contemporary European administrative standards. They are locked out of rapid implementation as long as the continuity reading frames the choice as cultural betrayal rather than technical modernization.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, reform_advocates, excluded,
    organized, biographical, constrained, national).

% The vast majority of Ottoman subjects who were illiterate and had no direct stake in script choice, but were excluded from literacy education that might have lifted them into the educated class. The continuity reading's demand that modernization preserve Arabic script raises the literacy barrier for the non-educated masses, who cannot learn to read without years of instruction in a complex orthography.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, rural_ottoman_population, excluded,
    powerless, biographical, trapped, regional).

% External analysts, historians, and comparative institutional scholars examining the structural costs and benefits of the orthographic choice. They observe the constraint in operation and measure the extraction dynamics across the seated parties.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, analytical_observer, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(orthographic_kernel__continuity_reading, ottoman_literate_ulama).
narrative_ontology:fixing_cost_class(orthographic_kernel__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the unified transmission of Islamic textual tradition across the Ottoman Empire and the broader Islamic world, maintaining a single orthographic system for jurisprudence, theology, and classical learning. Prevents fragmentation into competing script-based reading communities that would risk loss of textual unity and interpretive coherence.
% TRANSFER_FUNCTION: Transfers literacy privilege and administrative authority to the Ottoman literate class and Islamic institutional authorities by making Arabic script mastery a prerequisite for participation in formal governance, law, and knowledge transmission. Simultaneously transfers opportunity cost (slower technological modernization, higher barriers to mass education) from the ulama to the state modernization apparatus and the rural illiterate population.
% ABSENT_VOICES: The rural Ottoman population had no seat in the script choice debate—they were structurally excluded from literacy entirely. Later, Turkish nationalist reformers who viewed the script choice as blocking technological parity with Europe were marginalized in early Ottoman debates but eventually gained institutional power, shifting the reading between periods.
% DISAPPEARANCE_RATIONALE: If the continuity reading were abandoned and replaced with rapid Latin-script adoption, the Ottoman state could have accelerated scientific and military modernization, literacy campaigns would have reached the rural population faster, and Ottoman administrative capacity would have aligned more quickly with contemporary European standards. The Islamic institutional authority would have fragmented: Arabic-script scholarship would persist in parallel channels (madrasas, Islamic learning centers) but would lose its status as the sole legitimate orthography for Ottoman state authority.
% FOUNDING_PROBLEM: In the early Ottoman period (14th–16th centuries), Arabic script was the established, globally-circulating medium for Islamic learning, law, and governance. Ottoman legitimacy depended on demonstrating continuity with Islamic tradition. Adopting a non-Arabic script would have raised questions about Islamic orthodoxy and disrupted the flow of knowledge from the broader Islamic world into Ottoman administration and scholarship.
% FOUNDING_PROBLEM_CORROBORATION: Ottoman historians and scholars of the period attested to the legitimacy concern—Islamic law and jurisprudence were authored in Arabic, and Ottoman judges and administrators needed direct access to classical texts. Later analysts (19th–20th century Turkish reformers, Ottoman comparative historians, and modern institutional analysis) contested whether the founding problem remained live after the 15th century or had been superseded by modernization imperatives. No external, neutral corroborating party existed within the Ottoman context; the corroboration comes from later historical scholarship showing that technological and military pressures mounted while the orthographic commitment remained in place.
narrative_ontology:disappearance_verdict(orthographic_kernel__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_kernel__continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_kernel__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(orthographic_kernel__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_kernel__continuity_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_kernel__continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(orthographic_kernel__continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(orthographic_kernel__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness metric (0.68 at interval end) reflects the constraint's dual character: genuine coordination function (preserving Islamic textual unity) paired with substantial opportunity costs (slower modernization, higher education barriers, professional opportunity scarcity outside the literate elite). The suppression metric (0.72) reflects the active enforcement required—not principally coercive violence, but institutional control over education, administrative appointments, and legitimacy certification. The theatrical component (0.41) captures the growing performative dimension: by the later Ottoman period, the continuity-preservation justification is invoked repeatedly while the actual enforcement mechanism has shifted to inertia and institutional sunk costs. The accessibility_collapse (0.78) is high because once someone is born into the Ottoman literate class, their options within the system are tightly constrained—they cannot participate in formal life without Arabic script mastery, and the alternative (leaving the Ottoman administrative world entirely) is inaccessible to most. Resistance (0.55) is moderate because reform advocates mount real pressure (Tanzimat and later modernization movements), but the constraint persists through institutional integration rather than overwhelming suppression—it is lived as natural law within the system even though alternatives exist.
 *
 * PERSPECTIVAL GAP:
 *   From the ulama institutional seat, the constraint appears as coordination it maintains by stewarding Islamic textual authority. From the state seat, it appears as institutional capture preventing state-led modernization. The divergence arises because the two seats have opposite exit structures (ulama gain professional monopoly from the status quo; modernizers lose institutional design flexibility). No directionality override is needed—the derivation from beneficiary/victim plus exit options produces the right d values naturally.
 *
 * DIRECTIONALITY LOGIC:
 *   The Ottoman literate ulama are structural beneficiaries—the constraint preserves their authority, prestige, and professional monopoly. They have institutional power and arbitrage exits (they could adopt Latin script, but doing so would sacrifice their claim to represent Islamic tradition and would dissolve their distinctive social role). Their directionality is low (~0.15–0.25): they collect from the constraint, face minimal exit pressure. The Ottoman literate administrative class carries primary victim status—they pay through identity-locking (cannot exit without abandoning their career, family legacy, and cultural position). They have moderate power (organized professionals) but identity_locked exits, so their directionality is high (~0.70–0.80): the constraint extracts substantially from their seat. The state modernization apparatus is a secondary victim (blocked reform path, constrained to work within the continuity frame), institutional power, constrained exits—directionality moderate-high (~0.65–0.75). The rural Ottoman population would benefit from mass education but are structurally excluded, so they are neither beneficiary nor victim of THIS constraint specifically (they pay for a different constraint: the general education system's inaccessibility). Islamic institutional authority (global seat) is a beneficiary with arbitrage exits: they could participate in a fragmented script-based learning ecosystem but they collect from unified orthography—low directionality (~0.20).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint carries a mandatrophy risk: the founding problem (preserving Islamic textual continuity amid Ottoman imperial fragmentation) was addressed by the Arabic-script commitment early in the Ottoman period (14th–16th centuries). By the 17th–18th centuries, as Ottoman military defeats mounted and scientific/technological gaps with Europe widened, the founding problem's urgency shifted—the state's primary problem became military and technological parity, not Islamic textual preservation. The constraint persisted through institutional inertia (the ulama's entrenched authority, the administrative class's identity-locking, the education system's dependence on Arabic script) rather than ongoing addressing of a live founding problem. However, the mandatrophy is NOT RESOLVED in this story's interval: the constraint does eventually collapse (Turkish script transition in 1928), but that collapse is external to the story interval (0–50) and represents a regime change (Atatürk's national rupture reading, not the continuity frame itself overcoming its mandatrophy). Within the interval, the theatrical component rises (0.25 → 0.41), signaling that the justifying function is increasingly decorative while the institutional structure persists—a signature mandatrophy dynamic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_selection_naturalness,
    'Is the continuity_reading a genuine organizing frame for Ottoman orthographic choice, or a post-hoc reconstruction by later preservationists?',
    'Textual analysis of Ottoman-period defenses of Arabic script (Ottoman administrative documents, ulama writings, state correspondence). If contemporary Ottoman actors explicitly framed the choice as continuity-preservation, the reading is authentic; if the framing appears only in later retrospectives, it is a constructed narrative imposed on historical actors.',
    'If the reading is post-hoc, the constraint story describes a later interpretive imposition rather than the actual mechanism in force during the Ottoman period. The extraction dynamics would be reframed from ''institutional authority preserving continuity'' to ''later preservationists mining history for legitimacy claims.'' The CS structure would shift from lineage to extraction, or require nested constraint stories across temporal layers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_naturalness, empirical, 'Whether the continuity_reading was an organizing frame at the time or a later reconstruction.').

omega_variable(
    suppression_mechanism_internalized_or_structural,
    'Is the Ottoman literate class''s commitment to Arabic script sustained by structural barriers (no alternative available, institutional monopoly on education) or by internalized identity fusion (the script is inseparable from Islamic scholarly identity)?',
    'Counterfactual analysis: if the Ottoman state had mandated Latin-script adoption in a specific region while maintaining Islamic institutional authority elsewhere, would the affected literate class have quickly learned to read and write in the new script while retaining their scholarly and administrative roles? Evidence from later Turkish script transition shows rapid adoption when the state enforced it, suggesting the barrier was largely structural/coercive rather than internalized. However, post-transition continued use of Arabic script in Islamic religious contexts suggests identity-locked affinity persists.',
    'If suppression is largely structural, the constraint''s persistence depends on active state enforcement and could collapse quickly under regime change. If suppression is internalized, the constraint persists even without external enforcement because the victims have fused their identities with the system. A reading that attributes suppression as mostly internalized would argue the extraction is milder (victims chose it) than the metrics suggest; a reading that attributes it to structure argues the extraction is severe (victims are coerced) and the metrics understate active enforcement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_or_structural, empirical, 'Whether suppression sustaining the constraint is structural or internalized identity fusion.').

omega_variable(
    kernel_reading_alternative_interpretation,
    'Could the Arabic-script commitment be coherently read NOT as continuity-preservation but as extraction-maintenance by a religious authority that benefits from script complexity and literacy scarcity?',
    'Comparative reading across all three sibling constraints: if the modernization_reading and rupture_reading''s ε values are significantly lower, and if the continuity_reading shows high extraction specifically from the state modernization apparatus, then the continuity frame is selection-consistent with extraction rather than genuine coordination.',
    'If the alternative reading (extraction-maintenance framing) is equally coherent, the ''reading'' classification becomes ambiguous—the same kernel (orthographic choice) admits a continuity reading AND an extraction reading, neither logically foreclosing the other. This would lower confidence in the cs_structure.reading_relations: the relations assume the readings are genuinely distinct coherent frames, not just evaluative glosses on the same structural fact. The true ε-invariance might require decomposing into two constraints (continuity_kernel__coordination vs. continuity_kernel__extraction) sharing the same referent but differing in what aspect is treated as primary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_alternative_interpretation, conceptual, 'Whether the continuity_reading is a coherent alternative to extraction-focused readings or a evaluative gloss on the same structural dynamics.').

omega_variable(
    founding_problem_obsolescence_timing,
    'When did the founding problem (preserving Islamic textual continuity via Arabic script) shift from live to dead? Was it the 18th-century military defeats, the 19th-century Tanzimat reforms, the 20th-century Turkish nationalist turn, or a more gradual attrition?',
    'Periodization analysis of Ottoman state documents, reform proposals, and elite debates across centuries. Track when reformers stopped framing Latin-script adoption as ''cultural betrayal'' and started framing it as ''technical necessity for parity with Europe.'' If the shift is sharp and datable, it marks the moment the founding problem''s legitimacy eroded.',
    'Early obsolescence (18th century) would suggest the constraint persisted for 2+ centuries after its justifying problem died—a strong signal of extraction-via-institutional-inertia (piton candidate). Later obsolescence (19th–20th century) would suggest the problem remained contestable longer, supporting the tangled_rope claim that coordination and extraction remained intertwined.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence_timing, empirical, 'The historical moment when the founding problem''s legitimacy eroded versus when the constraint''s enforcement continued.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_kernel__continuity_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_tr_t0, orthographic_kernel__continuity_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(orth_tr_t0, observed).
narrative_ontology:measurement(orth_tr_t10, orthographic_kernel__continuity_reading, theater_ratio, 10, 0.29).
narrative_ontology:measurement_basis(orth_tr_t10, observed).
narrative_ontology:measurement(orth_tr_t20, orthographic_kernel__continuity_reading, theater_ratio, 20, 0.34).
narrative_ontology:measurement_basis(orth_tr_t20, observed).
narrative_ontology:measurement(orth_tr_t30, orthographic_kernel__continuity_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement_basis(orth_tr_t30, observed).
narrative_ontology:measurement(orth_tr_t40, orthographic_kernel__continuity_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement_basis(orth_tr_t40, observed).
narrative_ontology:measurement(orth_tr_t50, orthographic_kernel__continuity_reading, theater_ratio, 50, 0.41).
narrative_ontology:measurement_basis(orth_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(orth_be_t0, orthographic_kernel__continuity_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(orth_be_t0, observed).
narrative_ontology:measurement(orth_be_t10, orthographic_kernel__continuity_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(orth_be_t10, observed).
narrative_ontology:measurement(orth_be_t20, orthographic_kernel__continuity_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement_basis(orth_be_t20, observed).
narrative_ontology:measurement(orth_be_t30, orthographic_kernel__continuity_reading, base_extractiveness, 30, 0.66).
narrative_ontology:measurement_basis(orth_be_t30, observed).
narrative_ontology:measurement(orth_be_t40, orthographic_kernel__continuity_reading, base_extractiveness, 40, 0.67).
narrative_ontology:measurement_basis(orth_be_t40, observed).
narrative_ontology:measurement(orth_be_t50, orthographic_kernel__continuity_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(orth_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(orth_su_t0, orthographic_kernel__continuity_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(orth_su_t0, observed).
narrative_ontology:measurement(orth_su_t10, orthographic_kernel__continuity_reading, suppression_requirement, 10, 0.61).
narrative_ontology:measurement_basis(orth_su_t10, observed).
narrative_ontology:measurement(orth_su_t20, orthographic_kernel__continuity_reading, suppression_requirement, 20, 0.67).
narrative_ontology:measurement_basis(orth_su_t20, observed).
narrative_ontology:measurement(orth_su_t30, orthographic_kernel__continuity_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement_basis(orth_su_t30, observed).
narrative_ontology:measurement(orth_su_t40, orthographic_kernel__continuity_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(orth_su_t40, observed).
narrative_ontology:measurement(orth_su_t50, orthographic_kernel__continuity_reading, suppression_requirement, 50, 0.72).
narrative_ontology:measurement_basis(orth_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_kernel__continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(orthographic_kernel__continuity_reading, 0.1).
narrative_ontology:affects_constraint(orthographic_kernel__continuity_reading, orthographic_kernel__modernization_reading).
narrative_ontology:affects_constraint(orthographic_kernel__continuity_reading, orthographic_kernel__rupture_reading).
narrative_ontology:affects_constraint(orthographic_kernel__continuity_reading, ottoman_administrative_capacity_bottleneck).
narrative_ontology:affects_constraint(orthographic_kernel__continuity_reading, islamic_institutional_authority_persistence).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the orthographic_kernel. The kernel admits three structurally distinct readings with different ε values and directionality profiles: continuity_reading (high extraction from modernizers, preserved by institutional inertia), modernization_reading (extraction reframed as opportunity cost), and rupture_reading (extraction reframed as intentional subordination to be overthrown). Each reading is authored as a separate constraint story linked via network.affects_constraints. The shared referent is the Ottoman commitment to Arabic script; the divergence is in what the commitment means (coordination, extraction, obstacle) and therefore what the ε value measures and what the prescribed remedy is. See commentary.kernel_context for the three-way reading structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
