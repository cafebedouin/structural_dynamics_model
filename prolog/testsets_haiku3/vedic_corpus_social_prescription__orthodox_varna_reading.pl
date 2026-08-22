% ============================================================================
% CONSTRAINT STORY: vedic_corpus_social_prescription__orthodox_varna_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: vedic_corpus_social_prescription__orthodox_varna_reading
 *   human_readable: Vedic Varna Hierarchy as Divine Cosmic Prescription
 *   domain: religious/social/hermeneutic
 *
 * SUMMARY:
 *   The orthodox Brahminical reading of Vedic texts (Rig Veda, Yajur Veda,
 *   Upanishads, and later Dharmashastra) interprets the prescribed Varna
 *   hierarchy as divinely mandated cosmic order. The constraint is the claim
 *   and the enforcement machinery that maintains it: that texts literally
 *   prescribe occupational, marital, and ritual restrictions on Shudra and
 *   Dalit castes as necessary for cosmic harmony. This reading treats the
 *   Vedic varna formula (Brahmin priesthood, Kshatriya kingship, Vaishya
 *   commerce, Shudra service) as timeless law rather than historical artifact
 *   or spiritual metaphor. The constraint's persistence depends on textual
 *   authority (Brahminical interpretation monopoly), social enforcement
 *   (caste rules, ritual denial, marriage restrictions, occupational
 *   lockdown), and theological legitimation (the claim that varna hierarchy
 *   reflects the cosmic order itself). From the orthodox reading's seat, this
 *   is a natural law of cosmic order. From the beneficiary and victim seats,
 *   the metrics reveal a high-extraction snare: extractiveness rising from
 *   past coercive intensity (0.88 to 0.81 as legal enforcement weakened over
 *   the interval), theater rising as religious performance replaces coercive
 *   compliance (0.28 to 0.41, the increasingly ritualized defense of
 *   boundaries), and suppression remaining extremely high (0.87–0.91) because
 *   the mechanism persists through internalized identity, institutional
 *   gatekeeping, and periodic coercive enforcement.
 *
 * KEY AGENTS:
 *   - Brahmin caste — institutional beneficiary and agenda-setter; maintains textual authority and derives ritual monopoly, status, and economic resources from the prescribed order; identity-locked into the beneficiary role by religious cosmology
 *   - Shudra caste — prescribed to serve the three superior varnas; barred from Vedic study, ritual participation, property accumulation, and marriage outside varna; labor extracted without reciprocal protection; identity-locked by birth classification
 *   - Dalit communities — positioned outside the varna system entirely by orthodox reading (untouchable); excluded from temples, water wells, ritual participation; assigned polluting occupations; extraction combines labor appropriation with ritual humiliation and systematic exclusion
 *   - Orthodox Brahminical authorities — institutional guardians of Vedic interpretation; enforce textual orthodoxy through commentary (smriti) and ritual legitimacy adjudication; authority and identity depend on constraint persistence
 *   - Reformist interpreters — excluded from orthodox authority; argue for spiritual/metaphorical readings and caste abolition; resistance met with doctrinal rejection and social ostracism
 *   - Vedic texts corpus (non-agent) — historical objects whose semantic content is contested; the orthodox reading claims they literally prescribe varna hierarchy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_corpus_social_prescription__orthodox_varna_reading, 0.81).
domain_priors:suppression_score(vedic_corpus_social_prescription__orthodox_varna_reading, 0.87).
domain_priors:theater_ratio(vedic_corpus_social_prescription__orthodox_varna_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 0.87).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_corpus_social_prescription__orthodox_varna_reading, snare).
narrative_ontology:human_readable(vedic_corpus_social_prescription__orthodox_varna_reading, "Vedic Varna Hierarchy as Divine Cosmic Prescription").
narrative_ontology:topic_domain(vedic_corpus_social_prescription__orthodox_varna_reading, "religious/social/hermeneutic").

domain_priors:requires_active_enforcement(vedic_corpus_social_prescription__orthodox_varna_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_corpus_social_prescription__orthodox_varna_reading, '40fd4963-0a74-4f08-b31c-676da0a87cb6').
narrative_ontology:cs_kernel_codification('40fd4963-0a74-4f08-b31c-676da0a87cb6', formalized).
narrative_ontology:cs_authority_grounding('40fd4963-0a74-4f08-b31c-676da0a87cb6', lineage).
narrative_ontology:cs_interpretation_layer_present('40fd4963-0a74-4f08-b31c-676da0a87cb6').
narrative_ontology:cs_reading_relation('40fd4963-0a74-4f08-b31c-676da0a87cb6', vedic_corpus_social_prescription__reformist_spiritual_reading, forecloses).
narrative_ontology:cs_reading_relation('40fd4963-0a74-4f08-b31c-676da0a87cb6', vedic_corpus_social_prescription__colonial_orientalist_reading, coexists_with).
narrative_ontology:cs_axiom('40fd4963-0a74-4f08-b31c-676da0a87cb6', foundational, varna_literal_cosmic_prescription).
narrative_ontology:cs_axiom_status(varna_literal_cosmic_prescription, holdable).
narrative_ontology:cs_axiom_grounding('40fd4963-0a74-4f08-b31c-676da0a87cb6', varna_literal_cosmic_prescription, deontological).
narrative_ontology:cs_axiom('40fd4963-0a74-4f08-b31c-676da0a87cb6', secondary, brahminical_textual_authority_monopoly).
narrative_ontology:cs_axiom_status(brahminical_textual_authority_monopoly, overridden).
narrative_ontology:cs_axiom_grounding('40fd4963-0a74-4f08-b31c-676da0a87cb6', brahminical_textual_authority_monopoly, conventional).
narrative_ontology:cs_reference_frame('40fd4963-0a74-4f08-b31c-676da0a87cb6', cosmic_order_varna_reflection).
narrative_ontology:cs_drift_state('40fd4963-0a74-4f08-b31c-676da0a87cb6', contemporary_post_independence_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('40fd4963-0a74-4f08-b31c-676da0a87cb6', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(vedic_corpus_social_prescription__orthodox_varna_reading, vedic_corpus_social_prescription).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_caste).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__orthodox_varna_reading, shudra_caste).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__orthodox_varna_reading, dalit_communities).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__orthodox_varna_reading, vedic_divine_authority).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__orthodox_varna_reading, cosmic_order_immutability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the textual canon, performs ritual authority certification, controls interpretation of Vedic texts, and derives social status, ritual monopolies, and economic resources (land grants, tax exemptions, labor claims) from the prescribed order. Exit from beneficiary role is identity-dissolving: Brahmin identity is constituted through Varna superiority and ritual purity claims. Sets agendas through control of textual interpretation and priestly authority.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_caste, agenda_setter,
    institutional, civilizational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_caste, beneficiary).

% Prescribed to serve the three superior varnas without recourse; barred from Vedic study, ritual participation, and property accumulation; labor extracted without reciprocal ritual or legal protection. Marriage outside varna forbidden; occupational mobility structurally blocked. Identity-locked by birth classification, religious cosmology, and legal enforcement. Exit would require rejecting the entire framework that names their existence — not available within the religious paradigm.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, shudra_caste, payer,
    powerless, generational, identity_locked, continental).

% Positioned outside the varna system entirely by orthodox reading (untouchable), excluded from temple entry, water wells, ritual participation; assigned occupations considered polluting (leatherwork, sanitation, cremation). Extraction combines labor appropriation with ritual humiliation and systematic exclusion from all mechanisms of social mobility or dignity. Trap is enforced by religious doctrine, social norms, and legal penalty for violation (historically: violence, social death, property confiscation).
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, dalit_communities, payer,
    powerless, generational, trapped, continental).

% Institutional guardians of Vedic interpretation; perform textual commentary (smriti) that locks the orthodox reading in place; adjudicate ritual legitimacy and caste legitimacy claims; enforce boundaries through ritual denial and social sanction. Identity and authority depend on the constraint's persistence and perceived naturality.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, orthodox_brahminical_authorities, agenda_setter,
    institutional, civilizational, identity_locked, continental).

% Hindu reformers, Brahmo Samaj intellectuals, and contemporary Vedic scholars who argue for spiritual/metaphorical readings and caste abolition. Structurally excluded from orthodox textual authority; would reframe the Vedic texts as spiritual rather than prescriptive if heard. Resistance is met with doctrinal rejection, authority denial, and (historically) social ostracism.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, reformist_interpreters, excluded,
    organized, biographical, constrained, continental).

% Nineteenth-century British Indology codified Vedic texts as timeless 'Hindu law,' naturalizing Varna hierarchy for administrative governance. They externally validate the constraint's fixity while remaining outside the religious framework that generates and sustains it. Codification served colonial governance efficiency.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, colonial_administrators, observer,
    powerful, biographical, arbitrage, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_caste).
narrative_ontology:fixing_cost_class(vedic_corpus_social_prescription__orthodox_varna_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes ritual labor and societal functions hierarchically: Brahmins perform priesthood and teaching, Kshatriyas provide kingship and military, Vaishyas provide commerce and agriculture, Shudras provide service labor — each varna's prescribed occupation is claimed to be coordinated for societal wholeness.
% TRANSFER_FUNCTION: Extracts labor, deference, and material resources from Shudra and Dalit communities without reciprocal legal protection, ritual access, or property rights. Brahmin caste captures land grants, tax exemptions, ritual monopolies, marriage alliance status, and the authority to certify legitimacy itself. The extraction is justified as divinely mandated cosmic necessity rather than as social choice.
% ABSENT_VOICES: Shudra and Dalit communities, whose lived experience contradicts the divine-harmony narrative, are barred from textual interpretation and public theological disputation. Women's voices are marginalized (some texts acknowledge female rituality but restrict it; women are assigned duties to fathers/husbands/sons). Reformist and egalitarian interpretations are excluded from orthodox authority structures.
% DISAPPEARANCE_RATIONALE: If the orthodox varna reading lost institutional authority — if texts were reinterpreted spiritually or reformist readings captured canonical authority — social hierarchies and occupational restrictions would reorganize. Land holdings, ritual monopolies, caste-based marriage alliances, and the entire legitimacy structure for untouchability would lose their theological anchor. The reorganization has been ongoing since 19th-century reform movements and 20th-century constitutional equality clauses.
% FOUNDING_PROBLEM: How should cosmic order be maintained? The orthodox reading claims the Vedic varna prescription answers this: each varna fulfills its dharmic duty, and social order follows from cosmic order reflected in the human hierarchy.
% FOUNDING_PROBLEM_CORROBORATION: Orthodox Brahminical authorities continue to affirm the founding problem as live. However, independent historical scholarship, reformist Hindu theologians, and the constitutional framing of India (explicitly rejecting caste hierarchy) attest that the founding problem — the need for divinely prescribed hierarchy to maintain cosmic order — has been superseded. Dalit scholars document that the 'cosmic harmony' narrative serves only the beneficiaries; actual social function has operated through coercion and exclusion, not balance.
narrative_ontology:disappearance_verdict(vedic_corpus_social_prescription__orthodox_varna_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_corpus_social_prescription__orthodox_varna_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_corpus_social_prescription__orthodox_varna_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vedic_corpus_social_prescription__orthodox_varna_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_corpus_social_prescription__orthodox_varna_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.81 at interval end, 0.88 at start) because the constraint extracts labor, ritual participation, marriage rights, occupational mobility, and legal personhood from Shudra and Dalit communities without reciprocal benefit, and the extraction is justified as natural law rather than human choice. Suppression is extremely high (0.87–0.91) because the constraint's persistence requires continuous active enforcement: Brahminical gatekeeping of textual interpretation, legal penalties for caste-crossing, ritual denial of temple entry and water-well access, and internalized shame/identity-fusion that makes exit psychologically/spiritually impossible. Theater ratio rises from 0.28 to 0.41 over the interval because legal enforcement mechanisms (caste courts, occupational restrictions) weakened with colonial law and post-independence constitution, so institutional persistence increasingly depends on performative ritual maintenance and theological reaffirmation rather than direct coercion — the same constraint, but defended by increasingly performative means. The slight downward drift in extractiveness (0.88 to 0.81) reflects the legal-enforcement attrition: the constraint extracts less measurable labor as statutory caste restrictions lose force, even though the theological claim persists at full intensity. Accessibility collapse is high (0.79) because alternatives — reformist readings, egalitarian reinterpretations, constitutional equality — exist and are visible, but the orthodox reading's institutional authority, coupled with identity-fusion mechanisms, makes them largely inaccessible to those born into Shudra/Dalit positions. Resistance is substantial (0.68) because reform movements, Dalit activism, and post-1947 constitutional commitment to equality represent continuous push-back against the constraint's authority.
 *
 * PERSPECTIVAL GAP:
 *   The orthodox Brahminical seat experiences this constraint as natural law — cosmic order reflected in the human hierarchy. The Shudra and Dalit seats experience it as pure extraction defended by coercion. The engine computes per-seat divergence from the structural data (power, exit, beneficiary/victim); the authored claim is the structural truth of how the constraint actually operates (snare), not the beneficiary's self-description.
 *
 * DIRECTIONALITY LOGIC:
 *   Brahmin caste is the clear beneficiary (d near 0.0): receives land grants, tax exemptions, ritual monopoly, status authority, labor resources, and the ability to set the very agenda about what counts as legitimate interpretation. Institutional power + arbitrage-grade exit (can reinterpret texts, can migrate to new authority structures while remaining Brahmin) keeps d low. Shudra caste faces high extraction (d near 1.0): prescribed to serve without reciprocal protection, barred from property and ritual, constrained occupational mobility. Powerless position + identity-locked exit (cannot exit Shudra identity without spiritual death, cannot adopt Brahmin status) drives d high. Dalit communities face maximum extraction (d at 1.0): outside the varna system, assigned polluting labor, maximum suppression (trapped exit — no exit within the framework that defines them). Orthodox authorities are secondary beneficiaries and enforcement agents (d near 0.0): derive authority and legitimacy from constraint persistence; institutional power; identity-locked into agenda-setter role. Reformist interpreters are excluded (not modeled on the directionality scale, but their presence as organized resistance explains the resistance metric of 0.68).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how to maintain cosmic order — was real when the texts were composed (pre-2nd century BCE, contingent on a hierarchical ritual cosmology). By the 19th century, the problem had shifted: colonial law and reform movements provided alternative frameworks for social order without hereditary caste hierarchy. By the 21st century (interval end), the founding problem is dead in institutional law (India's constitution explicitly rejects caste as a basis for rights or privileges) but continues theatrically in religious practice and in communities where social enforcement persists. The constraint exhibits mandatrophy: the arrangement persists despite the founding problem's disappearance, maintained by institutional inertia (Brahminical textual authority), identity-fusion (Brahmin identity tied to superiority; Dalit identity tied to exclusion), and periodic coercive enforcement (communal violence, marriage restrictions, occupational gatekeeping in some regions). The rising theater_ratio (0.28 to 0.41) over the interval captures this: as legal enforcement weakened, the constraint increasingly depends on performative ritual reaffirmation — the same snare, defended by increasingly theatrical means.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vedic_text_semantic_underspecification,
    'Do the Vedic texts literally prescribe varna hierarchy, or do they describe a social ideal that can be reinterpreted metaphorically or spiritually?',
    'Textual semantics and hermeneutical analysis: does the Rig Veda 10.90 Purusha Sukta literally prescribe caste roles, or is it a cosmological poem open to non-prescriptive reading? This depends on reading conventions and what counts as literal.',
    'If metaphorical reading is defensible, the constraint''s claimed status as natural law collapses — it becomes a human interpretive choice benefiting Brahmins. The constraint would reclassify from ''natural law falsely claimed'' (mountain with beneficiaries → FSM → tangled_rope) to ''snare defended by ideological naturalization'' (confirmed snare). The alternative reading''s existence is the core omega: the constraint''s persistence depends on suppressing semantic alternatives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vedic_text_semantic_underspecification, conceptual, 'The constraint''s persistence depends on semantic foreclosure — preventing defensible alternative readings from entering the canonical interpretation space.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.87+) structural (legal enforcement, economic dependency, occupational lockdown) or internalized (Dalit/Shudra acceptance of the framework as cosmically legitimate)?',
    'Post-exit trajectories: Dalit/Shudra individuals who migrate to regions/communities where caste enforcement is weak or to countries without caste structure show whether suppression persists after structural barriers are removed. Lingering internalized shame, identity-fusion, or rapid normalization indicates the mechanism balance.',
    'If suppression is primarily structural, weakening legal enforcement (as happened 1947–present) should reduce the constraint''s extractiveness — which it has (0.88 to 0.81). If suppression is primarily internalized, the constraint persists more stubbornly despite legal change. The rising theater_ratio suggests increasing reliance on internalized mechanisms: legal enforcement decayed, so institutional persistence now depends on performative ritual + identity-fusion + community social enforcement. This shifts the suppression mechanism toward internalization over the interval.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs. internalized suppression: the balance determines whether legal reform suffices to dissolve the constraint or whether cultural/psychological deconditioning is also required.').

omega_variable(
    benefit_concentration_and_piton_boundary,
    'Does the Brahmin caste meaningfully benefit from the constraint''s continuation, or has institutional benefit become diffuse (a piton)?',
    'Land-holding data, ritual-monopoly enforcement, marriage-alliance control, and status asymmetries within the Brahmin caste: do contemporary Brahmin institutions and communities actively defend the constraint, or do they maintain it passively while modernizing? Generational exit from priesthood and ritual authority would indicate declining benefit concentration.',
    'If benefit remains concentrated in Brahminical institutional authorities (temples, textual gatekeepers, ritual controllers), the constraint is a snare with clear beneficiaries defending it. If benefit has become diffuse — Brahmins modernized and no longer defend caste restrictions, but the constraint persists by inertia and community enforcement — it becomes a piton. Historical trend: Brahmin caste has internally modernized and stratified (many Brahmins reject caste restrictions); institutional Brahminical authorities (temples, Sanskrit academies) continue to enforce orthodoxy. The constraint sits at the boundary: snare for institutional defenders, piton for many ordinary Brahmins who benefit incidentally but don''t actively defend it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(benefit_concentration_and_piton_boundary, empirical, 'Whether the constraint remains a snare with concentrated beneficiaries or has decayed into a piton maintained by institutional inertia.').

omega_variable(
    kernel_semantic_contest,
    'This story instantiates the ORTHODOX VARNA READING of the Vedic corpus kernel. What makes this reading the orthodox reading rather than the reformist or colonial readings?',
    'Institutional authority: which interpretation controls temple policy, textual commentary production, caste-legitimacy adjudication, and religious instruction? The orthodox reading is the one defended by the most powerful Brahminical institutions (major temple networks, scholarly lineages, orthodox schools of philosophy). This authority is historical and contingent, not necessary.',
    'The constraint''s persistence depends on this reading remaining institutionally dominant. If reformist reinterpretations captured institutional authority (temples, textual centers, educational institutions), the constraint would weaken and potentially dissolve. The colonial reading''s externalization (Victorian Orientalism fixing Varna as timeless Hindu law) reinforced the orthodox reading''s naturality claim by providing Western scholarly validation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_semantic_contest, conceptual, 'Committer frame: the orthodox reading''s authority is institutional and contestable, not natural or inevitable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_corpus_social_prescription__orthodox_varna_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t0, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(vedi_tr_t0, observed).
narrative_ontology:measurement(vedi_tr_t3, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 3, 0.32).
narrative_ontology:measurement_basis(vedi_tr_t3, observed).
narrative_ontology:measurement(vedi_tr_t6, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 6, 0.35).
narrative_ontology:measurement_basis(vedi_tr_t6, observed).
narrative_ontology:measurement(vedi_tr_t12, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 12, 0.39).
narrative_ontology:measurement_basis(vedi_tr_t12, observed).
narrative_ontology:measurement(vedi_tr_t19, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 19, 0.41).
narrative_ontology:measurement_basis(vedi_tr_t19, observed).
narrative_ontology:measurement(vedi_tr_t25, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(vedi_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(vedi_be_t0, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 0, 0.88).
narrative_ontology:measurement_basis(vedi_be_t0, observed).
narrative_ontology:measurement(vedi_be_t3, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 3, 0.86).
narrative_ontology:measurement_basis(vedi_be_t3, observed).
narrative_ontology:measurement(vedi_be_t6, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 6, 0.84).
narrative_ontology:measurement_basis(vedi_be_t6, observed).
narrative_ontology:measurement(vedi_be_t12, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 12, 0.83).
narrative_ontology:measurement_basis(vedi_be_t12, observed).
narrative_ontology:measurement(vedi_be_t19, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 19, 0.82).
narrative_ontology:measurement_basis(vedi_be_t19, observed).
narrative_ontology:measurement(vedi_be_t25, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 25, 0.81).
narrative_ontology:measurement_basis(vedi_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t0, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 0, 0.91).
narrative_ontology:measurement_basis(vedi_su_t0, observed).
narrative_ontology:measurement(vedi_su_t3, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 3, 0.9).
narrative_ontology:measurement_basis(vedi_su_t3, observed).
narrative_ontology:measurement(vedi_su_t6, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 6, 0.89).
narrative_ontology:measurement_basis(vedi_su_t6, observed).
narrative_ontology:measurement(vedi_su_t12, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 12, 0.88).
narrative_ontology:measurement_basis(vedi_su_t12, observed).
narrative_ontology:measurement(vedi_su_t19, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 19, 0.87).
narrative_ontology:measurement_basis(vedi_su_t19, observed).
narrative_ontology:measurement(vedi_su_t25, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 25, 0.87).
narrative_ontology:measurement_basis(vedi_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_corpus_social_prescription__orthodox_varna_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vedic_corpus_social_prescription__orthodox_varna_reading, 0.12).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__orthodox_varna_reading, vedic_corpus_social_prescription__reformist_spiritual_reading).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__orthodox_varna_reading, vedic_corpus_social_prescription__colonial_orientalist_reading).

% DUAL FORMULATION NOTE:
% The Vedic corpus social prescription is a contested kernel with three structurally distinct constraint stories: (1) orthodox_varna_reading — Vedic texts literally prescribe caste hierarchy as cosmic necessity; high-epsilon snare benefiting Brahmins, victimizing Shudra/Dalit. (2) reformist_spiritual_reading — Vedic texts describe spiritual unity and metaphorical cosmology; reinterprets varna as spiritual development stages, not occupational prescription; near-zero epsilon, rope-type coordination. (3) colonial_orientalist_reading — Vedic corpus codified as unified timeless 'Hindu law' for administrative governance; tangled_rope structure where colonial administrators benefit from legal codification while reaffirming Brahminical authority. The three readings are not observational variants of one constraint (ε-invariance principle): they have fundamentally different epsilon values (0.81 snare vs. ~0.08 rope vs. 0.45 tangled_rope) because they instantiate different beneficiary structures and different claims about what the texts prescribe. They are linked by semantic contest over the same kernel (the Vedic corpus) but are structurally distinct constraints with distinct narratives, victims, and persistence mechanisms. This story (orthodox reading) is the high-epsilon snare; its siblings are separate constraint stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
