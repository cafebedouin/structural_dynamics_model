% ============================================================================
% CONSTRAINT STORY: vedic_corpus_social_prescription__orthodox_varna_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_corpus_social_prescription__orthodox_varna_reading, []).

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
 *   constraint_id: vedic_corpus_social_prescription__orthodox_varna_reading
 *   human_readable: Vedic Varna Hierarchy as Divinely Mandated Cosmic Order (Orthodox Reading)
 *   domain: religious/social_stratification
 *
 * SUMMARY:
 *   The orthodox Vedic reading of varna hierarchy treats Sanskrit
 *   texts—particularly the Rig Veda's Purusha Sukta and Dharmashastra legal
 *   codes—as literal, divinely mandated cosmic prescription. Under this
 *   reading, the four-fold varna system (Brahmin/priest, Kshatriya/warrior,
 *   Vaishya/merchant, Shudra/servant, plus untouchable Dalits) is not a
 *   historical institution but an eternal cosmic order rooted in the body of
 *   the primordial cosmic person. Occupational heredity, ritual purity
 *   gradation, marriage restrictions, property rights, and political
 *   legitimacy all derive from this scriptural foundation. For Shudra and
 *   Dalit castes, the reading prescribes mandatory service, ritual exclusion,
 *   and a cognitive framework in which escape is cosmically impossible. This
 *   is one reading of a contested kernel: the Vedic corpus itself. Sibling
 *   readings (colonial_orientalist_reading, reformist_spiritual_reading)
 *   interpret the same texts differently, with radically different social
 *   consequences. This reading generates high extraction and suppression
 *   because the constraint's persistence depends on enforcing textual
 *   literalism and excluding alternative interpretations.
 *
 * KEY AGENTS:
 *   - brahmin_priesthood: Institutional beneficiary, agenda-setter; monopolizes Vedic interpretation and ritual authority.
 *   - kshatriya_ruling_class: Institutional beneficiary; legitimizes political dominance through Vedic authority while remaining subordinate to Brahmin spiritual authority.
 *   - shudra_castes: Powerless victims; identity-locked by birth; labor and ritual restrictions extract value while preventing escape.
 *   - dalit_castes: Powerless victims; ritually untouchable; exist outside varna structure as structurally polluting; legal restrictions compound ritual exclusion.
 *   - reform_and_anticolonial_critics: Excluded from textual interpretation; would argue for metaphorical reading or rejection of Vedic prescriptive authority if seated.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_corpus_social_prescription__orthodox_varna_reading, 0.82).
domain_priors:suppression_score(vedic_corpus_social_prescription__orthodox_varna_reading, 0.91).
domain_priors:theater_ratio(vedic_corpus_social_prescription__orthodox_varna_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_corpus_social_prescription__orthodox_varna_reading, snare).
narrative_ontology:human_readable(vedic_corpus_social_prescription__orthodox_varna_reading, "Vedic Varna Hierarchy as Divinely Mandated Cosmic Order (Orthodox Reading)").
narrative_ontology:topic_domain(vedic_corpus_social_prescription__orthodox_varna_reading, "religious/social_stratification").

domain_priors:requires_active_enforcement(vedic_corpus_social_prescription__orthodox_varna_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_corpus_social_prescription__orthodox_varna_reading, '7ab7482f-88b1-4c3d-a585-eac0095e48f1').
narrative_ontology:cs_kernel_codification('7ab7482f-88b1-4c3d-a585-eac0095e48f1', fixed_text).
narrative_ontology:cs_authority_grounding('7ab7482f-88b1-4c3d-a585-eac0095e48f1', extraction).
narrative_ontology:cs_interpretation_layer_present('7ab7482f-88b1-4c3d-a585-eac0095e48f1').
narrative_ontology:cs_reading_relation('7ab7482f-88b1-4c3d-a585-eac0095e48f1', vedic_corpus_social_prescription__reformist_spiritual_reading, forecloses).
narrative_ontology:cs_reading_relation('7ab7482f-88b1-4c3d-a585-eac0095e48f1', vedic_corpus_social_prescription__colonial_orientalist_reading, coexists_with).
narrative_ontology:cs_axiom('7ab7482f-88b1-4c3d-a585-eac0095e48f1', foundational, vedic_texts_literally_prescribe_varna).
narrative_ontology:cs_axiom_status(vedic_texts_literally_prescribe_varna, holdable).
narrative_ontology:cs_axiom_grounding('7ab7482f-88b1-4c3d-a585-eac0095e48f1', vedic_texts_literally_prescribe_varna, conventional).
narrative_ontology:cs_axiom('7ab7482f-88b1-4c3d-a585-eac0095e48f1', foundational, varna_immutable_across_lifetimes).
narrative_ontology:cs_axiom_status(varna_immutable_across_lifetimes, holdable).
narrative_ontology:cs_axiom_grounding('7ab7482f-88b1-4c3d-a585-eac0095e48f1', varna_immutable_across_lifetimes, theological).
narrative_ontology:cs_reference_frame('7ab7482f-88b1-4c3d-a585-eac0095e48f1', cosmic_varna_immutability).
narrative_ontology:cs_drift_state('7ab7482f-88b1-4c3d-a585-eac0095e48f1', contemporary_post_independence_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7ab7482f-88b1-4c3d-a585-eac0095e48f1', '').
narrative_ontology:cs_kernel_id(vedic_corpus_social_prescription__orthodox_varna_reading, vedic_corpus_social_prescription).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_priesthood).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__orthodox_varna_reading, shudra_castes).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__orthodox_varna_reading, dalit_castes).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__orthodox_varna_reading, kshatriya_ruling_class).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__orthodox_varna_reading, vaishya_merchant_class).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__orthodox_varna_reading, cosmic_order_immutability).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__orthodox_varna_reading, ritual_purity_gradation).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__orthodox_varna_reading, occupational_heredity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and transmits Vedic texts as literal prescription of hierarchical social order. Controls ritual performance, textual authority, and theological justification. Monopolizes Vedic study and transmission; claims ritual purity status dependent on birth. Directly benefits from occupational monopolies in ritual, scholarship, and intellectual authority. They frame varna as cosmic necessity, not human choice.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_priesthood, agenda_setter,
    institutional, civilizational, arbitrage, regional).

% Benefits from Brahmin legitimation of hierarchical rule; Vedic authority is invoked to justify political dominance. Their own place in the varna order is fixed above commoners but subordinate to Brahmin spiritual authority, creating a structural accommodation: they govern politically while Brahmins govern spiritually. Their occupational domain (governance, warfare) is protected by varna heredity.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, kshatriya_ruling_class, beneficiary,
    powerful, generational, mobile, regional).

% Occupy the third varna tier; entitled to agriculture, trade, and animal husbandry within hereditary occupational bounds. While constrained relative to Kshatriyas, they retain wealth accumulation and economic mobility within their prescribed sphere, differentiating them sharply from those below. Varna protects their economic domain from competition by lower castes.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, vaishya_merchant_class, beneficiary,
    powerful, generational, constrained, regional).

% Hereditary service castes. Vedic texts prescribe that they serve the three higher varnas; ritual law forbids them from Vedic study, ownership of land or animals beyond subsistence, participation in sacrificial rites, and marriage outside their stratum. They bear mandatory labor obligations, ritual exclusion, and cognitive barriers to identity change. Birth determines occupation, marriage, property rights, and ritual status. No appeal or escape exists within the framework; change would require rejecting the entire religious cosmology in which their identity is embedded.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, shudra_castes, payer,
    powerless, generational, identity_locked, regional).

% Positioned outside and below the four varnas as ritually polluting; historically associated with labor in pollution-producing occupations (leather work, cremation, waste handling). Vedic and later Dharmashastra texts mandate their permanent ritual exclusion and untouchability. Physical contact with them is treated as polluting; their testimony is invalid in court; their property rights are curtailed or nonexistent. They are born into this status and the texts declare it unchangeable. Even ritual purification is forbidden them.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, dalit_castes, payer,
    powerless, generational, identity_locked, regional).

% Scholarly and priestly bodies (Mimamsa, Vedanta, Dharmashastra commentators) that maintain orthodox interpretation and enforcement of Vedic varna doctrine. They articulate and defend the textual literalism that grounds the constraint. They are distinct from the Brahmin priesthood proper but represent and reinforce the same interpretive commitment. They produce the philosophical apparatus justifying the hierarchy.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, orthodox_vedic_authorities, observer,
    institutional, civilizational, analytical, regional).

% Hindu reformers (Brahmo Samaj, Arya Samaj), Dalit movements, independence leaders, and modern scholars who reject varna literalism or reject Vedic prescriptive authority entirely. They are structurally excluded from orthodox textual interpretation—their voices delegitimized as modern corruption, lack of spiritual authority, or anti-Hindu bias. Within the orthodox framework, their interpretive claims are inadmissible. If admitted to interpretation, they would argue for metaphorical reading, historical contextualization of texts, or explicit rejection of Vedic social prescriptions.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, reform_and_anticolonial_critics, excluded,
    organized, generational, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_priesthood).
narrative_ontology:fixing_cost_class(vedic_corpus_social_prescription__orthodox_varna_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint provides a metaphysical justification for occupational specialization and social stability: if every being's varna is cosmically ordained and immutable, then each performing their prescribed role maintains the cosmic order (rita/rta). Individuals are told they have no choice in occupation or status, which eliminates negotiation costs—everyone is supposed to accept their role as cosmic necessity. This appears to solve the coordination problem of organizing labor and maintaining hierarchy without explicit coercive negotiation at every point, though the actual solution depends entirely on suppression.
% TRANSFER_FUNCTION: Extracts labor value from Shudra and Dalit castes in the form of mandatory service (Shudras are prescribed to serve the three higher varnas; Dalits occupy even lower polluting labor), occupational restriction (hereditary binding to low-status work), property denial (ownership restrictions), and ritual exclusion (forbidden from sacred learning, ritual participation, and even physical proximity). Transfers the product of that labor to the three higher varnas and transfer of ritual authority, learning, and social esteem exclusively to the Brahmin priesthood. Additionally extracts cognitive freedom by making identity change structurally unthinkable within the framework (divinely ordained, immutable across lifetimes).
% ABSENT_VOICES: Shudra and Dalit intellectual voices are constitutively excluded from Vedic interpretation within the orthodox reading's framework—their Vedic knowledge is forbidden by varna law, their textual commentary is inadmissible (testimony from low castes is invalid), their spiritual insight is denied authority. If they were seated at interpretation with equal standing, they would argue: (1) the Purusha Sukta is metaphorical cosmology, not social prescription; (2) varna rigidity is historical, not eternal; (3) Vedic texts contain passages denying varna hierarchy (Bhakti hymns, certain Upanishadic passages); (4) the literal reading was selectively emphasized by Brahmin authorities to maintain power, not discovered in timeless texts. Reform movements making exactly these arguments are structurally locked out of orthodox textual authority.
% DISAPPEARANCE_RATIONALE: The orthodox varna reading directly justifies occupational restriction, inheritance laws, marriage law, property rights, ritual participation, legal testimony, and political legitimacy across the pre-modern Hindu world and continues to structure institutions, law, and social practice in the modern Indian subcontinent. If this reading disappeared—i.e., if orthodox interpretation shifted to deny that Vedic texts literally prescribe varna—the entire structural justification for caste restrictions, occupational monopolies, ritual boundaries, and legal discrimination would collapse. Labor would reorganize around open competition rather than hereditary assignment. Property law would require renegotiation. Marriage law would need re-codification. Brahmin monopolies on ritual and learning would lose their cosmological warrant and face pressure to open. Dalit political movements would gain textual ground for their claims of equality. The world would rearrange substantially.
% FOUNDING_PROBLEM: Vedic texts (especially Rig Veda Purusha Sukta, composed ~1200 BCE) contain a creation myth describing the cosmic body (purusha) divided into four functional parts: Brahmins emerge from the mouth (speech, learning), Kshatriyas from the arms (power, protection), Vaishyas from the torso (sustenance, production), Shudras from the feet (service). Later Dharmashastra texts (Manusmrti, Yajnavalkyasmrti, composed 200 BCE–500 CE) elaborate this into a comprehensive legal and social code prescribing occupational heredity, marriage restrictions, property rules, and ritual boundaries for each varna. Orthodox interpreters took these texts literally to assert that varna is a divinely mandated, eternally binding social reality rooted in cosmic order itself. The founding problem, from their perspective, is: how does society maintain cosmic order and prevent chaos (kaliyuga)? Answer: varna hierarchy, which is not a human invention but divine prescription.
% FOUNDING_PROBLEM_CORROBORATION: Orthodox Brahmin and Kshatriya authorities and orthodox Vedic scholars attest that the founding problem remains live: varna is immutable cosmic law, violation of varna causes cosmic disorder and social chaos, Vedic texts literally prescribe and justify the hierarchy. They cite: continuity of caste practice over millennia, philosophical elaborations in Vedanta schools, ritual texts that differentiate varna-based participation, and appeals to the cosmic order (rita). Reformist Hindu scholars (Keshab Chandra Sen, Rammohan Roy, modern Dalit scholars including B.R. Ambedkar) and academic Vedic philologists attest (from OUTSIDE the benefiting parties) that: (1) the Purusha Sukta is a late Vedic metaphorical elaboration, not a direct social prescription; (2) pre-varna occupational systems existed and changed across early Vedic texts; (3) occupational mobility and caste fluidity appear in historical records from various periods; (4) the literal interpretation was selectively emphasized by Brahmin authorities during periods of institutional consolidation (roughly 500 BCE–500 CE) and institutional maintenance thereafter; (5) other Vedic passages (Bhakti hymns in Upanishads, some Samaveda verses, Rig Veda X.34 and others) explicitly deny caste hierarchy and affirm spiritual equality. The mismatch between orthodox claims of timeless prescription and historical-critical evidence documenting textual layers, institutional power consolidation, and competing textual voices is the contested core.
narrative_ontology:disappearance_verdict(vedic_corpus_social_prescription__orthodox_varna_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_corpus_social_prescription__orthodox_varna_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_corpus_social_prescription__orthodox_varna_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(vedic_corpus_social_prescription__orthodox_varna_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_corpus_social_prescription__orthodox_varna_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vedic_corpus_social_prescription__orthodox_varna_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vedic_corpus_social_prescription__orthodox_varna_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the reading's literal interpretation directly justifies occupational restriction, labor extraction, property denial, and cognitive closure—the material mechanisms of caste oppression. Suppression is very high (0.91) because the constraint depends on enforcing textual literalism and actively excluding reformist, metaphorical, and historical-critical readings. The orthodoxy must suppress Dalit intellectual production, reformist theology, and textual scholarship that treats the texts as historically contingent rather than eternally prescriptive. Theater ratio rises over the measurement interval (0.25→0.42) because as material caste restrictions faced mounting internal and external pressure (especially post-18th century), orthodox defenders increasingly invested in ritual and philosophical theater—scholastic reinterpretation, elaborate justification, appeal to cosmic harmony—to defend the constraint's legitimacy without yielding material ground. The measurements show extractiveness rising through 1000 years (0→1000) as the constraint institutionalized across law, ritual, and property systems, then stabilizing at a high plateau (1000→2000) as the system matured. Suppression follows a similar trajectory: rising as orthodoxy had to defend against heterodox challenges (Buddhism, Jainism, Bhakti movements) and maintain boundaries, then stabilizing at a very high level once institutional consolidation locked competing interpretations out of official channels. Theater rises as a proportion of enforcement activity because the material mechanisms of caste hardened over time, requiring more ideological and ritual justification relative to pure force.
 *
 * PERSPECTIVAL GAP:
 *   From the Brahmin priesthood and orthodox authorities' institutional position, the constraint is a literal, unchangeable cosmic truth—disagreement with it is spiritual ignorance or willful rebellion, not legitimate interpretation. From Shudra and Dalit positions, the same texts and institutions operate as extractive domination, with their exclusion from Vedic knowledge serving to prevent them from arguing back against the constraint in its own terms. The engine computes these divergent types from the structural data: the beneficiary seat (Brahmin priesthood) and payer seats (Shudra/Dalit) should produce different classifications because their directionality differs radically—one benefits from the constraint's existence and maintenance, the other bears its costs and has no exit except collective resistance. The oracle deriving d from beneficiary/victim declarations and exit options should show Brahmin priesthood d near 0.1 (beneficiary, mobile institutional exit through reinterpretation), while Shudra/Dalit d should be near 0.95 (victims, identity-locked, no exit within the framework). This directionality asymmetry is the engine's measurement of how the constraint operates entirely differently depending on where you sit.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: brahmin_priesthood. Vedic literal authority is the justification for their monopoly on ritual, their superior social status, their exclusive right to interpret texts and define cosmic order. They benefit from the constraint because it grounds their institutional power. Their exit options are mobile at the institutional level—orthodoxy could reinterpret texts, drop literalism, or redistribute interpretive authority—but the Brahmin caste structure depends on maintaining the constraint, so individual exit is rare and collective exit is institutional suicide. d ≈ 0.15 (beneficiaries with some constrained mobility). Victims: shudra_castes and dalit_castes. They pay the constraint through occupational restriction, labor extraction, ritual exclusion, property denial, legal incompetence, and identity closure. Their exit options are identity_locked: the constraint treats varna as hereditary and unchangeable, rooted in cosmic law. Escape requires either rejecting the framework entirely (which means leaving the religious and social world within which the constraint is defined) or collectively overthrowing the constraint from outside it. d ≈ 0.95 (full targets, deeply trapped). Kshatriya and Vaishya classes sit between: they benefit from varna subordination of the classes below them and from Brahmin legitimation of their rule, but they are themselves constrained by varna rules (restrictions on Vedic study, subordination to Brahmin spiritual authority, restrictions on occupational mobility). Their exit is constrained rather than mobile or identity_locked, and their benefit is real but asymmetric. For them, d ≈ 0.35-0.45.
 *
 * MANDATROPHY ANALYSIS:
 *   The orthodoxy reading does not satisfy the mandatrophy criteria for misclassification. The founding problem (cosmic order requires social hierarchy rooted in varna) remains live in orthodox discourse and is actively maintained. Suppression and extraction measurements show no decay consistent with atrophied mandate. What DOES appear in the measurements is rising theater_ratio alongside stable extraction: this suggests that the material mechanisms of caste (property law, occupation restriction, ritual practice) are stable and extractive, while the ideological justification (cosmic order, spiritual necessity) increasingly requires elaborate reinterpretation to address reformist challenges. This is not mandatrophy—it is not an atrophied constraint maintained by inertia. It is an active, contested constraint whose ruling coalition must do increasing interpretive work to maintain legitimacy, even as they maintain the material extraction intact. The distinction matters: mandatrophy would signal that the constraint persists despite losing function; here, the constraint's function (legitimating caste oppression) remains fully operative, and the increasing theater ratio signals only that the opposition to the constraint has grown loud enough to require more sophisticated defense.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_literalism_vs_metaphor,
    'Is the Purusha Sukta and its varna cosmology meant as literal prescription for social organization, or as metaphorical/mythological elaboration of functional specialization?',
    'Historical-critical Vedic philology comparing textual layers, contemporaneous non-varna occupational texts, and evidence of varna fluidity in early Vedic period; examination of when literal prescriptive readings became dominant in orthodox commentary traditions.',
    'If the texts are primarily metaphorical, the constraint reclassifies from snare to false-summit mountain (a natural cosmology misread as justifying social hierarchy). If literal, the snare classification is confirmed. The evidence weight falls on historical contingency: varna rigidity increased over centuries, not present in earliest texts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_literalism_vs_metaphor, empirical, 'Whether Vedic varna doctrine is inherent textual content or later orthodox interpretation.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'To what extent is the suppression of Shudra and Dalit resistance structural (legal barriers, property denial, exclusion from literacy and Vedic learning) versus internalized (cognitive acceptance of cosmic inferiority, self-enforcing shame, religious indoctrination)?',
    'Observation of resistance behavior when structural barriers are legally removed (post-independence India: Dalit uplift constitutionally mandated but resistance remains high); measurement of post-exit suppression (if Dalits emigrate or convert, does suppression persist in new contexts, indicating internalization).',
    'If largely structural, the constraint''s power is tied to institutional enforcement; removing legal barriers should significantly reduce extraction. If largely internalized, the constraint exhibits deep cognitive capture: removal of legal barriers alone is insufficient, and exit capacity remains low even when formally free. The evidence suggests both: structural barriers are primary (enabling the initial extraction), but centuries of enforcement have produced internalized shame, caste identity fusion, and spiritual self-blame (suicide rates among Dalits facing discrimination even in legally equal contexts).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Internalized vs. structural components of caste suppression.').

omega_variable(
    orthodox_authority_strategic_reinterpretation,
    'Is the rising theater_ratio (0.25→0.42) evidence that orthodox authorities are deliberately reinterpreting the texts to soften prescriptive readings while maintaining material caste restrictions, or is it a side effect of defensive posturing against reform movements?',
    'Examination of orthodox commentary tradition (Mimamsa, Vedanta schools) over the measurement interval: do reinterpretations explicitly soften social prescriptions while maintaining cosmic varna doctrine? Do reformulations correlate with moments of high reform pressure (Bhakti movements, British reform critique)?',
    'If strategic reinterpretation, the constraint exhibits active capture: authorities maintain extraction while adjusting the justification narrative to co-opt reformist criticism. If defensive posturing, the rising theater_ratio indicates the constraint''s ideological legitimacy is eroding under pressure. Either way, the theater ratio rise is significant: it indicates the gap between material extraction (stable) and ideological justification (increasingly elaborate) is widening.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(orthodox_authority_strategic_reinterpretation, empirical, 'Whether rising theater reflects strategic reframing or defensive legitimation under pressure.').

omega_variable(
    identity_lock_fusion_boundaries,
    'What mechanisms maintain identity_locked status for Shudra and Dalit castes? Is it legal restriction (non-transferable property, non-exogamous marriage), religious doctrine (karma/rebirth justifying current caste as earned status), cognitive capture (internalized untouchability), or combination?',
    'Ethnographic and historical evidence of escape attempts and their barriers; post-legal-reform data on caste mobility; evidence of dissociative spiritual movements (Bhakti, Sikhism, Buddhism, conversion) that explicitly reject caste identity and their success/suppression rates.',
    'If primarily legal, changing law should increase exit; if primarily religious-cognitive, legal change alone is insufficient. The evidence suggests deep fusion: Dalits who legally ''escape'' via education or employment face continued discrimination; conversion to non-Hindu religions is often used as actual exit but is suppressed (discriminatory anti-conversion laws). The identity is extraordinarily sticky even when formal barriers are removed, indicating internalization is severe.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identity_lock_fusion_boundaries, empirical, 'Composition and depth of identity-lock mechanisms binding Shudra/Dalit castes.').

omega_variable(
    vedic_authority_vs_local_practice_drift,
    'To what degree do local caste practices (marriage, occupation, ritual) actually conform to prescriptions in the classical Vedic/Dharmashastra texts versus local variation and heterodox adaptation?',
    'Comparative ethnography of regional caste systems; historical records of local practice deviating from textual prescription; evidence of competing local cosmologies or reinterpretations that soften varna rigidity.',
    'If local practice closely tracks textual prescription, the orthodox reading''s authority is reinforced—the texts genuinely shape behavior. If substantial drift exists, the constraint is more institutional/customary than textually prescribed, and the orthodox reading is more a post-hoc narrative than a generative rule. Evidence suggests significant regional and temporal variation: occupational mobility existed in some regions/periods; some castes had marriage flexibility; some regions had non-hereditary service arrangements. The texts are more of a legitimating framework for existing hierarchies than a prescriptive generator.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vedic_authority_vs_local_practice_drift, empirical, 'Fit between textual varna prescriptions and actual regional caste practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_corpus_social_prescription__orthodox_varna_reading, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t0, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(vedi_tr_t0, projected).
narrative_ontology:measurement(vedi_tr_t250, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 250, 0.3).
narrative_ontology:measurement_basis(vedi_tr_t250, observed).
narrative_ontology:measurement(vedi_tr_t500, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 500, 0.35).
narrative_ontology:measurement_basis(vedi_tr_t500, observed).
narrative_ontology:measurement(vedi_tr_t1000, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 1000, 0.42).
narrative_ontology:measurement_basis(vedi_tr_t1000, observed).
narrative_ontology:measurement(vedi_tr_t1500, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 1500, 0.41).
narrative_ontology:measurement_basis(vedi_tr_t1500, observed).
narrative_ontology:measurement(vedi_tr_t2000, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 2000, 0.42).
narrative_ontology:measurement_basis(vedi_tr_t2000, observed).

% Extraction over time
narrative_ontology:measurement(vedi_be_t0, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 0, 0.78).
narrative_ontology:measurement_basis(vedi_be_t0, projected).
narrative_ontology:measurement(vedi_be_t250, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 250, 0.81).
narrative_ontology:measurement_basis(vedi_be_t250, observed).
narrative_ontology:measurement(vedi_be_t500, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 500, 0.83).
narrative_ontology:measurement_basis(vedi_be_t500, observed).
narrative_ontology:measurement(vedi_be_t1000, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 1000, 0.84).
narrative_ontology:measurement_basis(vedi_be_t1000, observed).
narrative_ontology:measurement(vedi_be_t1500, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 1500, 0.82).
narrative_ontology:measurement_basis(vedi_be_t1500, observed).
narrative_ontology:measurement(vedi_be_t2000, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 2000, 0.82).
narrative_ontology:measurement_basis(vedi_be_t2000, observed).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t0, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement_basis(vedi_su_t0, projected).
narrative_ontology:measurement(vedi_su_t250, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 250, 0.88).
narrative_ontology:measurement_basis(vedi_su_t250, observed).
narrative_ontology:measurement(vedi_su_t500, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 500, 0.9).
narrative_ontology:measurement_basis(vedi_su_t500, observed).
narrative_ontology:measurement(vedi_su_t1000, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 1000, 0.92).
narrative_ontology:measurement_basis(vedi_su_t1000, observed).
narrative_ontology:measurement(vedi_su_t1500, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 1500, 0.91).
narrative_ontology:measurement_basis(vedi_su_t1500, observed).
narrative_ontology:measurement(vedi_su_t2000, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 2000, 0.91).
narrative_ontology:measurement_basis(vedi_su_t2000, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_corpus_social_prescription__orthodox_varna_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vedic_corpus_social_prescription__orthodox_varna_reading, 0.12).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__orthodox_varna_reading, vedic_corpus_social_prescription__reformist_spiritual_reading).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__orthodox_varna_reading, vedic_corpus_social_prescription__colonial_orientalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested kernel 'vedic_corpus_social_prescription.' The kernel is the Vedic texts themselves; the reading_id distinguishes this particular interpretation (literal cosmic prescription) from reformist readings (spiritual/metaphorical, no social prescription) and colonial readings (administrative codification). The three constraints share the same primary texts but decompose along interpretive lines, yielding different ε values, victim sets, and structural mechanisms. The orthodox reading (this file) shows high extraction because literal interpretation directly justifies caste oppression. The reformist reading shows low extraction because metaphorical interpretation denies social prescriptive content. The colonial reading shows moderate extraction because codification serves British administrative interest rather than purely brahmin interest. All three readings compete for authority over the Vedic corpus; they coexist as live positions held by different institutional and political factions. This constraint influences both sibling readings because the orthodox literal interpretation, when it holds authority, creates structural pressure against both reformist metaphorization (spirituality threatens social stability, authorities claim) and colonial codification (Vedic truth is eternal, not administrative construct). Link all three constraints in network.affects_constraints to model the interpretive contestation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
