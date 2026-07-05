% ============================================================================
% CONSTRAINT STORY: treaty_authority_cession__rangatiratanga_retention_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_treaty_authority_cession__rangatiratanga_retention_reading, []).

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
 *   constraint_id: treaty_authority_cession__rangatiratanga_retention_reading
 *   human_readable: Te Tiriti Partnership Reading — Rangatiratanga Retention via Contra Proferentem
 *   domain: constitutional_law/indigenous_rights/colonial_history
 *
 * SUMMARY:
 *   This story instantiates the rangatiratanga-retention reading of the
 *   contested treaty_authority_cession kernel: the Māori-text-controls,
 *   partnership-requiring-consent reading of te Tiriti o Waitangi. Under this
 *   reading, kāwanatanga names a limited governance authority ceded to the
 *   Crown, tino rangatiratanga (full chiefly authority over lands, villages,
 *   and taonga) is expressly retained, and Crown action affecting Māori
 *   interests is legitimate only insofar as it proceeds from ongoing consent
 *   rather than unilateral assertion. This produces a coordination structure
 *   — genuine division of governmental labour between Crown and hapū/iwi —
 *   that functions as intended when honoured (Rope-like) but that becomes
 *   visibly extractive precisely where the Crown's historical conduct
 *   (raupatu, the Native Land Court, resource legislation) departed from the
 *   consent standard this reading establishes; those departures are captured
 *   here as victim relationships rather than as a separate constraint,
 *   because they are failures to comply with THIS reading's own terms, not a
 *   different bargain. This is deliberately distinct from the sibling
 *   crown_cession_reading (English text controls, kāwanatanga equals full
 *   sovereignty) and from retrospective_snare_exposure (which treats the
 *   textual divergence itself, independent of any partnership framing, as the
 *   extraction mechanism). Each is a separate constraint with its own ε and
 *   its own network link.
 *
 * KEY AGENTS:
 *   - hapu_and_iwi_signatories: retained tino rangatiratanga, bear costs when partnership is breached
 *   - crown_when_acting_with_consent: holds kāwanatanga, legitimate only with consent
 *   - hapu_and_iwi_under_unilateral_crown_action: victims of breach under this reading's own standard
 *   - maori_land_owners_under_historic_alienation: victims of historical land alienation reinterpreted as breach
 *   - waitangi_tribunal: analytical/adjudicative observer applying this reading
 *   - settler_descendant_general_population: excluded from original bargain, materially affected by outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__rangatiratanga_retention_reading, 0.38).
domain_priors:suppression_score(treaty_authority_cession__rangatiratanga_retention_reading, 0.55).
domain_priors:theater_ratio(treaty_authority_cession__rangatiratanga_retention_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__rangatiratanga_retention_reading, tangled_rope).
narrative_ontology:human_readable(treaty_authority_cession__rangatiratanga_retention_reading, "Te Tiriti Partnership Reading — Rangatiratanga Retention via Contra Proferentem").
narrative_ontology:topic_domain(treaty_authority_cession__rangatiratanga_retention_reading, "constitutional_law/indigenous_rights/colonial_history").

domain_priors:requires_active_enforcement(treaty_authority_cession__rangatiratanga_retention_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__rangatiratanga_retention_reading, '375013c0-5242-48d8-aea1-a41d99d559fd').
narrative_ontology:cs_kernel_codification('375013c0-5242-48d8-aea1-a41d99d559fd', fixed_text).
narrative_ontology:cs_authority_grounding('375013c0-5242-48d8-aea1-a41d99d559fd', lineage).
narrative_ontology:cs_interpretation_layer_present('375013c0-5242-48d8-aea1-a41d99d559fd').
narrative_ontology:cs_reading_relation('375013c0-5242-48d8-aea1-a41d99d559fd', treaty_authority_cession__crown_cession_reading, forecloses).
narrative_ontology:cs_reading_relation('375013c0-5242-48d8-aea1-a41d99d559fd', treaty_authority_cession__retrospective_snare_exposure, influences).
narrative_ontology:cs_axiom('375013c0-5242-48d8-aea1-a41d99d559fd', foundational, contra_proferentem_maori_text_controls).
narrative_ontology:cs_axiom_status(contra_proferentem_maori_text_controls, holdable).
narrative_ontology:cs_axiom_grounding('375013c0-5242-48d8-aea1-a41d99d559fd', contra_proferentem_maori_text_controls, conventional).
narrative_ontology:cs_axiom('375013c0-5242-48d8-aea1-a41d99d559fd', foundational, crown_authority_conditioned_on_ongoing_consent).
narrative_ontology:cs_axiom_status(crown_authority_conditioned_on_ongoing_consent, holdable).
narrative_ontology:cs_axiom_grounding('375013c0-5242-48d8-aea1-a41d99d559fd', crown_authority_conditioned_on_ongoing_consent, deontological).
narrative_ontology:cs_axiom('375013c0-5242-48d8-aea1-a41d99d559fd', secondary, kawanatanga_limited_to_governance_function).
narrative_ontology:cs_axiom_status(kawanatanga_limited_to_governance_function, holdable).
narrative_ontology:cs_axiom_grounding('375013c0-5242-48d8-aea1-a41d99d559fd', kawanatanga_limited_to_governance_function, empirically_contingent).
narrative_ontology:cs_reference_frame('375013c0-5242-48d8-aea1-a41d99d559fd', bilateral_partnership_compact_1840).
narrative_ontology:cs_drift_state('375013c0-5242-48d8-aea1-a41d99d559fd', post_waitangi_tribunal_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('375013c0-5242-48d8-aea1-a41d99d559fd', '').
narrative_ontology:cs_kernel_id(treaty_authority_cession__rangatiratanga_retention_reading, treaty_authority_cession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(treaty_authority_cession__rangatiratanga_retention_reading, hapu_and_iwi_signatories).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__rangatiratanga_retention_reading, crown_when_acting_with_consent).
narrative_ontology:constraint_victim(treaty_authority_cession__rangatiratanga_retention_reading, hapu_and_iwi_under_unilateral_crown_action).
narrative_ontology:constraint_victim(treaty_authority_cession__rangatiratanga_retention_reading, maori_land_owners_under_historic_alienation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(treaty_authority_cession__rangatiratanga_retention_reading, hapu_and_iwi_signatories).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Signed te Tiriti o Waitangi in te reo Māori, ceding kāwanatanga (governance) while retaining tino rangatiratanga (chiefly authority) over lands, villages, and taonga. Under this reading, the Māori text controls and the Crown's authority to act is conditioned on ongoing consent and partnership, not unilateral sovereignty. They benefit when the partnership model is honoured — co-governance, consultation, restored authority over resources — and bear costs whenever the Crown treats kāwanatanga as if it were full cession, since the same text is then read against them. Exit from the treaty relationship is not available; they can litigate, negotiate, or protest, but cannot leave the polity the treaty constitutes.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, hapu_and_iwi_signatories, beneficiary,
    organized, civilizational, constrained, national).
narrative_ontology:stakeholder_secondary_role(treaty_authority_cession__rangatiratanga_retention_reading, hapu_and_iwi_signatories, payer).

% Exercises governance authority (kāwanatanga) over the general population and administers the state. Under this reading, its legitimacy to act on matters touching Māori interests depends on negotiated consent from hapū and iwi — the Treaty of Waitangi settlement process, co-governance arrangements, and statutory consultation duties are read as fulfilling this obligation rather than as discretionary concessions. The Crown benefits from a stable constitutional narrative that grounds its authority in a genuine bilateral compact rather than a bare act of conquest, and can shift interpretive posture (contra proferentem favouring the Māori text, or asserting Crown sovereignty as settled) depending on forum and stakes.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, crown_when_acting_with_consent, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(treaty_authority_cession__rangatiratanga_retention_reading, crown_when_acting_with_consent, beneficiary).

% Where the Crown has legislated, confiscated, or regulated without securing consent, this reading identifies those actions as breaches of the partnership rather than as valid exercises of ceded sovereignty. The costs are historical (raupatu confiscations, the Native Land Court, twentieth-century resource legislation) and ongoing (resource management, foreshore and seabed, fisheries allocation). They cannot exit the constitutional relationship; their only leverage is political mobilisation, Waitangi Tribunal claims, and litigation asserting the partnership standard against the Crown's actual conduct.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, hapu_and_iwi_under_unilateral_crown_action, payer,
    organized, generational, trapped, national).

% Individual and whānau landholders whose land passed out of Māori ownership through mechanisms — the Native Land Court's individualisation of title, compulsory acquisition, rating sales — that this reading treats as violations of tino rangatiratanga rather than legitimate exercises of Crown governance. This reading makes the historical extraction visible: under the Māori text, the Crown never acquired authority to dispose of land ownership structures, so alienation without consent is retrospectively a breach, not a lawful act. They bear the accumulated cost with no individual exit.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, maori_land_owners_under_historic_alienation, payer,
    powerless, generational, trapped, regional).

% A standing commission of inquiry that hears claims of Crown breach against the principles of the treaty, drawing heavily on the partnership/rangatiratanga-retention reading to assess historical and contemporary Crown conduct. It has no binding enforcement power over the Crown but produces findings that shape settlement negotiations and public understanding of which reading of the treaty is operative.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, waitangi_tribunal, observer,
    institutional, generational, analytical, national).

% The wider population governed under kāwanatanga, largely absent from treaty interpretation debates despite being materially affected by settlements, co-governance arrangements, and resource reallocation that flow from this reading. Their consent was never sought for the original bargain and they are rarely direct parties to Tribunal proceedings, though referenda and political contestation periodically bring their views into the process.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, settler_descendant_general_population, excluded,
    moderate, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(treaty_authority_cession__rangatiratanga_retention_reading, diffuse).
narrative_ontology:fixing_cost_class(treaty_authority_cession__rangatiratanga_retention_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a bilateral compact in which the Crown gains authority to govern (kāwanatanga) over the colony's general affairs while hapū and iwi retain authority over their own lands, resources, and internal affairs (tino rangatiratanga) — a genuine division-of-labour solution to the coordination problem of a settler state and existing polities occupying the same territory, provided the Crown's exercise of governance over Māori interests remains conditioned on consent.
% TRANSFER_FUNCTION: Under legitimate operation, authority over general governance functions (customs, external relations, law and order for the colony) moves from hapū/iwi to the Crown, while authority over land, resources, and internal Māori affairs remains with hapū/iwi. Where the Crown has acted unilaterally — legislating, confiscating, or reallocating without consent — the same instrument is read as having moved land and authority from Māori to the Crown and settlers without the partnership's terms being met.
% ABSENT_VOICES: The rangatira who signed did not have equal access to legal drafting resources and negotiated the Māori text without a mechanism to contest the English version's divergent wording at the time. Contemporary excluded voices include hapū whose specific historical grievances have not reached Tribunal hearing, and the general population who are rarely direct parties despite being affected by settlement outcomes.
% DISAPPEARANCE_RATIONALE: If this reading of the treaty were abandoned, the entire architecture of the Waitangi Tribunal, treaty settlements, co-governance arrangements, and the statutory principle of partnership embedded across dozens of New Zealand statutes would lose its constitutional grounding. Settlement negotiations, resource allocation regimes, and the legal basis for restored rangatiratanga over specific taonga would need a different justification or would collapse.
% FOUNDING_PROBLEM: In 1840 the Crown sought a legal basis to establish a colonial government and regulate escalating land transactions and settler conflict, while rangatira sought protection of their authority, lands, and taonga against unregulated settler encroachment and inter-hapū conflict exacerbated by contact. The treaty was the instrument through which both parties believed they were solving their respective problems — but the two language texts describe different bargains.
% FOUNDING_PROBLEM_CORROBORATION: Waitangi Tribunal findings, drawing on independent historical and linguistic scholarship (including work on 1840s Māori political concepts and the drafting history of Henry Williams's translation), corroborate that kāwanatanga was understood by signatories as limited governance rather than sovereign cession — this scholarship originates outside both Crown legal advisers and iwi claimant groups. The Crown's own historical Waitangi Tribunal responses have, since the 1980s, accepted elements of the partnership reading in principle, though contemporaneous nineteenth-century Crown officials disputed it.
narrative_ontology:disappearance_verdict(treaty_authority_cession__rangatiratanga_retention_reading, world_rearranges).
narrative_ontology:founding_problem_status(treaty_authority_cession__rangatiratanga_retention_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(treaty_authority_cession__rangatiratanga_retention_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(treaty_authority_cession__rangatiratanga_retention_reading, 'none', 1).
narrative_ontology:epsilon_provenance(treaty_authority_cession__rangatiratanga_retention_reading, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(treaty_authority_cession__rangatiratanga_retention_reading_tests).
:- end_tests(treaty_authority_cession__rangatiratanga_retention_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.38 at present) because under this reading the treaty's core structure is a genuine coordination solution — governance divided from internal authority — not a device built to extract. But it is not negligible, because much of New Zealand's actual constitutional and land history consists of Crown action that this reading identifies as unlawful breach, and that breach is real and ongoing in its consequences even though it is not what the constraint, properly read, authorises. Suppression sits at a moderate 0.55: contemporary suppression is lower than the raupatu-era peak but the Crown retains superior institutional capacity to define 'consent' unilaterally in specific instances. Theater ratio (0.42) reflects that a meaningful share of contemporary Crown 'partnership' activity — consultation processes, treaty principles clauses in legislation — functions partly as legitimating performance layered atop continuing asymmetric decision-making power, alongside real substantive change (settlements, co-governance) that is not merely performative. Accessibility collapse is moderate (0.4): the partnership framework is a real, actively litigated and negotiated alternative to unilateral Crown sovereignty, not a foreclosed possibility — hence lower than a mountain's near-total collapse. Resistance is high (0.72): hapū and iwi have continuously and organisedly contested departures from the consent standard for over 150 years, through petition, litigation, protest, and the Tribunal process itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Hapū and iwi signatories sit near the beneficiary end when the partnership is honoured — the whole structural point of retaining tino rangatiratanga is that authority over their own affairs remains with them. But the same group becomes the target when Crown conduct breaches the consent standard, which is why they carry a secondary payer role and why two further stakeholder groups (unilateral-action victims, historic land-alienation victims) are named separately with trapped exit and organized/powerless power respectively — their directionality is pulled hard toward the target end because the extraction they suffer is concrete, historical, and largely irreversible (land once alienated is not returned by reinterpretation alone). The Crown occupies a genuine dual position: legitimate governance authority when acting with consent (low d, near beneficiary) shading toward extractive agenda-setter when it has not (the Tribunal's finding record establishes numerous such instances). This dual position is exactly what a tangled_rope classification is built to hold: a real coordination function (governance/rangatiratanga division) coexisting with asymmetric extraction (breach) running through the identical structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents two mislabeling errors. First, it prevents flattening the entire treaty relationship into pure extraction (which the retrospective_snare_exposure sibling reading is built to examine on its own terms) — doing so would erase the genuine partnership function this reading identifies and would make the Tribunal's entire jurisprudential apparatus, built on the principle that breach is a departure from a real standard, unintelligible. Second, it prevents treating the partnership as costlessly benign coordination (the crown_cession_reading's mirror error in the other direction) — doing so would erase 150+ years of documented breach and the ongoing material harm to Māori land owners and hapū under unilateral Crown action. The tangled_rope classification holds both facts simultaneously: coordination function is real and victims are real, and both ride the same textual and constitutional structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_standard_operationalisation,
    'What specific evidentiary standard determines whether the Crown has secured ''consent'' from hapū/iwi sufficient to legitimate a given exercise of governance authority under this reading — unanimous hapū agreement, majority iwi authority endorsement, or Tribunal-certified adequate consultation?',
    'Comparative analysis of Tribunal findings and settlement negotiation practice across multiple claims to identify the de facto standard being applied, cross-checked against what rangatira in 1840 would have understood ''partnership'' to require.',
    'A stricter consent standard would reclassify a much larger share of twentieth-century Crown legislative action as breach (raising the effective extraction and victim count under this reading); a looser standard would narrow the victim set to only the most flagrant unilateral seizures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_standard_operationalisation, conceptual, 'How strictly the consent condition is operationalised determines the scope of what counts as breach under this reading.').

omega_variable(
    which_reading_the_state_actually_operates_under,
    'Which of the three kernel readings (crown_cession, rangatiratanga_retention, retrospective_snare_exposure) does the New Zealand state''s actual constitutional practice track at any given moment — and does this vary by domain (resource law vs. criminal law vs. constitutional reform)?',
    'Domain-by-domain audit of statutory treaty principles clauses, court rulings on treaty interpretation, and executive practice, mapped against which reading each instance implicitly assumes.',
    'If state practice tracks crown_cession_reading in most domains while rhetorically invoking rangatiratanga_retention_reading, the theater_ratio authored here understates the gap between this reading''s normative claim and actual Crown conduct — the constraint would be more accurately characterised as legitimating cover for the sibling reading''s operative extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(which_reading_the_state_actually_operates_under, empirical, 'Whether Crown practice actually implements this reading or merely invokes it rhetorically while operating under crown_cession_reading.').

omega_variable(
    translation_intent_ambiguity,
    'Did Crown officials in 1840 (specifically Hobson and the missionary translators) understand the Māori and English texts to diverge in substance, or did they believe kāwanatanga and ''sovereignty'' were equivalent concepts in good faith?',
    'Historical-linguistic analysis of 1840s correspondence, missionary records, and contemporaneous Māori political vocabulary; this bears on whether the divergence was negligent, deliberate, or a genuine cross-cultural translation failure.',
    'Deliberate divergence would support treating the founding instrument itself as having been extractive from inception (pulling this constraint toward the retrospective_snare_exposure sibling''s territory); good-faith translation failure supports treating this reading as the legitimate original bargain later violated by subsequent unilateral Crown action.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(translation_intent_ambiguity, empirical, 'Whether the English/Māori textual divergence originated in Crown intent or genuine translation limitation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__rangatiratanga_retention_reading, 1840, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trea_tr_t1840, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1840, 0.15).
narrative_ontology:measurement(trea_tr_t1865, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1865, 0.35).
narrative_ontology:measurement(trea_tr_t1900, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1900, 0.55).
narrative_ontology:measurement(trea_tr_t1940, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1940, 0.5).
narrative_ontology:measurement(trea_tr_t1975, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1975, 0.4).
narrative_ontology:measurement(trea_tr_t2000, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 2000, 0.42).
narrative_ontology:measurement(trea_tr_t2025, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(trea_be_t1840, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1840, 0.2).
narrative_ontology:measurement(trea_be_t1865, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1865, 0.68).
narrative_ontology:measurement(trea_be_t1900, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1900, 0.72).
narrative_ontology:measurement(trea_be_t1940, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1940, 0.6).
narrative_ontology:measurement(trea_be_t1975, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1975, 0.45).
narrative_ontology:measurement(trea_be_t2000, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 2000, 0.36).
narrative_ontology:measurement(trea_be_t2025, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 2025, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(trea_su_t1840, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 1840, 0.3).
narrative_ontology:measurement(trea_su_t1865, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 1865, 0.8).
narrative_ontology:measurement(trea_su_t1900, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 1900, 0.75).
narrative_ontology:measurement(trea_su_t1940, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 1940, 0.65).
narrative_ontology:measurement(trea_su_t1975, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 1975, 0.55).
narrative_ontology:measurement(trea_su_t2000, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 2000, 0.5).
narrative_ontology:measurement(trea_su_t2025, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 2025, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(treaty_authority_cession__rangatiratanga_retention_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(treaty_authority_cession__rangatiratanga_retention_reading, 0.12).
narrative_ontology:affects_constraint(treaty_authority_cession__rangatiratanga_retention_reading, crown_cession_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__rangatiratanga_retention_reading, retrospective_snare_exposure).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the treaty_authority_cession kernel. crown_cession_reading holds the English text controls and full sovereignty was ceded — a structurally different constraint with a much smaller victim set and higher claimed legitimacy for unilateral Crown action. retrospective_snare_exposure treats the textual divergence itself as the extraction mechanism, independent of which reading 'wins,' and carries the highest ε of the three because it locates extraction in the founding instrument's construction rather than in departures from a legitimate standard. This story (rangatiratanga_retention_reading) sits between them: it authors a genuine coordination function while also registering the historical breaches as victim relationships internal to its own standard. All three share the founding 1840 event and should be read together, never averaged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(treaty_authority_cession__rangatiratanga_retention_reading, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
