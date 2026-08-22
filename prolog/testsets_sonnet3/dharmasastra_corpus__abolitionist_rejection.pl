% ============================================================================
% CONSTRAINT STORY: dharmasastra_corpus__abolitionist_rejection
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dharmasastra_corpus__abolitionist_rejection, []).

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
 *   constraint_id: dharmasastra_corpus__abolitionist_rejection
 *   human_readable: Dharmasastra Corpus as Standing Arrangement — Abolitionist Reading
 *   domain: religious_law/textual_interpretation/normative_authority
 *
 * SUMMARY:
 *   This story instantiates the abolitionist reading of the Dharmasastra
 *   kernel: the classical Hindu legal-textual corpus (Manusmriti and related
 *   texts) governing varna/jati hierarchy, personal law, and ritual duty.
 *   Under this reading, the corpus retains no legitimate authority of any
 *   kind — not as revealed literal law (the orthodox_literalist reading) and
 *   not as an ethical core separable from caste prescriptions (the
 *   reformist_contextual reading). The abolitionist reading holds that the
 *   extractive hierarchy IS the corpus's operative content, not a corrigible
 *   accretion on top of a salvageable ethical teaching, and that dismantling
 *   the hierarchy dismantles the victim set along with it rather than
 *   redistributing status within it. Extraction is authored high (0.87)
 *   because this reading treats the standing arrangement — the corpus as
 *   currently cited in customary practice, personal law claims, and social
 *   sanction — as substantially oppressive; theater ratio (0.40) captures the
 *   increasing use of 'cultural heritage' and 'spiritual tradition' framing
 *   to preserve social function once literal legal authority has been
 *   constitutionally curtailed. This is a sibling of
 *   dharmasastra_corpus__orthodox_literalist and
 *   dharmasastra_corpus__reformist_contextual — same kernel, structurally
 *   distinct claims, per the ε-invariance principle. Each carries its own ε
 *   and stakeholder set; they are linked via network.affects_constraints and
 *   are not merged here.
 *
 * KEY AGENTS:
 *   - brahmin_priestly_class: interpretive custodians and primary beneficiaries (institutional/arbitrage) — collect status and gatekeeping authority
 *   - dominant_caste_landholders: material beneficiaries of labor/marriage ordering (powerful/mobile)
 *   - dalit_communities: primary targets of untouchability and exclusion provisions (powerless/trapped)
 *   - shudra_laborers, women_under_customary_family_law, inter_caste_couples, adivasi_communities: differentiated victim groups bearing distinct extraction mechanisms
 *   - constitutional_courts_and_state: analytical observer adjudicating the tension between constitutional equality and customary claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_corpus__abolitionist_rejection, 0.87).
domain_priors:suppression_score(dharmasastra_corpus__abolitionist_rejection, 0.82).
domain_priors:theater_ratio(dharmasastra_corpus__abolitionist_rejection, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, extractiveness, 0.87).
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__abolitionist_rejection, snare).
narrative_ontology:human_readable(dharmasastra_corpus__abolitionist_rejection, "Dharmasastra Corpus as Standing Arrangement — Abolitionist Reading").
narrative_ontology:topic_domain(dharmasastra_corpus__abolitionist_rejection, "religious_law/textual_interpretation/normative_authority").

domain_priors:requires_active_enforcement(dharmasastra_corpus__abolitionist_rejection).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__abolitionist_rejection, 'ff6f986e-9d15-484a-ae66-e8b3343e57c4').
narrative_ontology:cs_kernel_codification('ff6f986e-9d15-484a-ae66-e8b3343e57c4', fixed_text).
narrative_ontology:cs_authority_grounding('ff6f986e-9d15-484a-ae66-e8b3343e57c4', lineage).
narrative_ontology:cs_interpretation_layer_present('ff6f986e-9d15-484a-ae66-e8b3343e57c4').
narrative_ontology:cs_reading_relation('ff6f986e-9d15-484a-ae66-e8b3343e57c4', dharmasastra_corpus__orthodox_literalist, forecloses).
narrative_ontology:cs_reading_relation('ff6f986e-9d15-484a-ae66-e8b3343e57c4', dharmasastra_corpus__reformist_contextual, coexists_with).
narrative_ontology:cs_axiom('ff6f986e-9d15-484a-ae66-e8b3343e57c4', foundational, corpus_possesses_zero_legitimate_authority).
narrative_ontology:cs_axiom_status(corpus_possesses_zero_legitimate_authority, holdable).
narrative_ontology:cs_axiom_grounding('ff6f986e-9d15-484a-ae66-e8b3343e57c4', corpus_possesses_zero_legitimate_authority, deontological).
narrative_ontology:cs_axiom('ff6f986e-9d15-484a-ae66-e8b3343e57c4', foundational, hierarchy_and_ethical_content_are_inseparable).
narrative_ontology:cs_axiom_status(hierarchy_and_ethical_content_are_inseparable, holdable).
narrative_ontology:cs_axiom_grounding('ff6f986e-9d15-484a-ae66-e8b3343e57c4', hierarchy_and_ethical_content_are_inseparable, conventional).
narrative_ontology:cs_reference_frame('ff6f986e-9d15-484a-ae66-e8b3343e57c4', brahmanical_lineage_transmission_authority).
narrative_ontology:cs_drift_state('ff6f986e-9d15-484a-ae66-e8b3343e57c4', post_constitutional_abolition_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('ff6f986e-9d15-484a-ae66-e8b3343e57c4', '').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__abolitionist_rejection, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__abolitionist_rejection, brahmin_priestly_class).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__abolitionist_rejection, dominant_caste_landholders).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__abolitionist_rejection, temple_and_matha_institutions).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, dalit_communities).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, shudra_laborers).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, women_under_customary_family_law).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, inter_caste_couples).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, adivasi_communities_pressured_into_hierarchy).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__abolitionist_rejection, varna_ashrama_dharma_doctrine).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__abolitionist_rejection, ritual_purity_hierarchy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Historically the interpretive custodians of the Dharmasastra corpus; occupy the top of the ritual-purity hierarchy the texts codify. Continue to derive social deference, ritual-officiant income, and institutional gatekeeping authority (temple appointments, personal-law interpretation, educational curation) from the corpus's continued treatment as authoritative. Can reposition as 'cultural heritage custodians' if literal authority is challenged, preserving much of the benefit under a softer label.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, brahmin_priestly_class, beneficiary,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(dharmasastra_corpus__abolitionist_rejection, brahmin_priestly_class, agenda_setter).

% Benefit from the labor-allocation and marriage-endogamy functions the varna/jati system enforces at the village and district level — land tenure patterns, bonded and semi-bonded agricultural labor, and social sanction against inter-caste land or marriage claims all trace back to caste ordering the corpus supplies religious cover for. Can exit into secular property law and market relations if hierarchy softens; the underlying wealth base persists even if the textual justification collapses.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, dominant_caste_landholders, beneficiary,
    powerful, generational, mobile, regional).

% Institutional bodies (temples, mathas, dharmic trusts) that administer endowments, control priestly appointments along caste lines, and cite Dharmasastra provisions to justify entry restrictions and ritual role allocation. Hold legal personhood and litigate to preserve customary practice against constitutional equality claims; well-resourced to survive a shift in interpretive authority.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, temple_and_matha_institutions, beneficiary,
    institutional, civilizational, arbitrage, national).

% Historically classified as outside or beneath the varna order, subjected to untouchability, occupational restriction, and settlement segregation with direct textual grounding in Dharmasastra provisions on impurity and pollution. Constitutional abolition of untouchability exists on paper but customary practice, backed by community and institutional sanction citing the same texts, persists in marriage, temple entry, land access, and local labor markets. Exit requires migration away from village social structures, conversion, or reliance on state enforcement that is inconsistently applied.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, dalit_communities, payer,
    powerless, generational, trapped, national).

% Positioned as the servile fourth varna whose dharma is service to the higher three; textually barred from Vedic study, priesthood, and many forms of independent economic and ritual authority. Bear the labor-extraction function of the hierarchy while occupying a marginally less stigmatized position than Dalits. Mobility exists through education and urban migration but is resisted by continued social citation of caste duty.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, shudra_laborers, payer,
    powerless, generational, constrained, regional).

% Subject to Dharmasastra provisions on inheritance exclusion, marital subordination (pativrata norms), remarriage restriction, and property rights that were only partially superseded by statutory personal law reform; customary and community-level enforcement of the older textual norms persists alongside statute, especially in inheritance and widowhood. Formal legal exit exists but carries severe social cost.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, women_under_customary_family_law, payer,
    powerless, biographical, constrained, national).

% Face social ostracism, family violence including honor killing, and community sanction justified by reference to caste-endogamy as dharmic duty. Have no meaningful exit within the community structure; formal law protects the marriage but does not protect against extralegal community enforcement of the hierarchy the corpus underwrites.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, inter_caste_couples, payer,
    powerless, immediate, trapped, local).

% Indigenous communities historically outside the varna framework who face pressure toward Sanskritized incorporation into the hierarchy at low rank, or exclusion and land dispossession justified by treating their customary status as outside dharmic civilization. Bear costs of hierarchy's expansion without ever having consented to the framework's premises.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, adivasi_communities_pressured_into_hierarchy, payer,
    powerless, generational, constrained, regional).

% Argue the ethical core of dharma is separable from time-bound caste prescriptions and seek to reinterpret rather than abandon the tradition. From the abolitionist reading's standpoint they are not part of this constraint's resolution — their reformist project is treated as a separate, coexisting claim rather than incorporated here — but their voices are structurally present in the same discourse space and would object that wholesale abandonment forecloses a live reconstructive possibility.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, reformist_scholars_and_movements, excluded,
    moderate, generational, constrained, national).

% Adjudicate the tension between constitutional equality guarantees and customary/personal-law claims rooted in the corpus; have abolished untouchability and enacted anti-discrimination statute but leave much personal law and social practice to community self-governance, producing a documented but unevenly enforced record of the harms this reading identifies.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, constitutional_courts_and_state, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dharmasastra_corpus__abolitionist_rejection, diffuse).
narrative_ontology:fixing_cost_class(dharmasastra_corpus__abolitionist_rejection, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None recognized by this reading as genuine at the level of the corpus itself: the hierarchy's proponents claim it coordinates social role, ritual duty, and cosmic order into a stable division of labor, but this reading holds that claimed coordination is the extraction mechanism's cover story, not a real solved problem — the same social stability could be, and increasingly is, achieved without caste ranking.
% TRANSFER_FUNCTION: Moves ritual status, land, labor, marriage-market position, and access to sacred and civic space from those classified low or outside the varna order to those classified high within it, using textual sanction to make the transfer appear cosmically ordained rather than socially imposed.
% ABSENT_VOICES: Reformist and contextualist voices are present in the wider discourse but excluded from this reading's resolution by design — this reading holds that their reconstructive project itself under-counts victims by preserving the hierarchy's textual authority in softened form. More acutely absent are the historical victims themselves across centuries, whose testimony was not preserved by an interpretive tradition controlled by the beneficiary class.
% DISAPPEARANCE_RATIONALE: If the corpus's authority vanished overnight — no community, court, or institution any longer treated it as source of legitimate obligation — inheritance practice, temple entry, marriage sanction, and occupational stigma tied to caste status would lose their primary textual justification. Beneficiary institutions (temples, mathas, dominant-caste social sanction networks) would need to justify continued stratification purely on secular power grounds, without religious cover, which this reading expects would substantially weaken their capacity to enforce it.
% FOUNDING_PROBLEM: Ancient and classical-era compilers of Dharmasastra texts presented themselves as systematizing right conduct (dharma) for social stability and cosmic order across caste, gender, and life-stage roles, addressing questions of succession, ritual obligation, and social duty in a period without unified secular law.
% FOUNDING_PROBLEM_CORROBORATION: Indian constitutional framers (Ambedkar prominently, himself from an oppressed caste, writing and speaking outside the Brahmanical interpretive tradition) attested that the founding problem's supposed solution was itself the instrument of oppression and that no legitimate social-ordering function survived independent of caste hierarchy; this corroboration comes from a framer who was a direct target of the arrangement, not from beneficiary-class commentary. Contemporary Dalit rights organizations and constitutional courts corroborate continuing harm from citation of the corpus in customary practice, independent of orthodox or reformist religious authorities.
narrative_ontology:disappearance_verdict(dharmasastra_corpus__abolitionist_rejection, world_rearranges).
narrative_ontology:founding_problem_status(dharmasastra_corpus__abolitionist_rejection, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dharmasastra_corpus__abolitionist_rejection, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dharmasastra_corpus__abolitionist_rejection, 'none', 1).
narrative_ontology:epsilon_provenance(dharmasastra_corpus__abolitionist_rejection, 0.87, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dharmasastra_corpus__abolitionist_rejection_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dharmasastra_corpus__abolitionist_rejection, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dharmasastra_corpus__abolitionist_rejection_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.87 reflecting this reading's assessment that the corpus's standing social function is overwhelmingly extractive rather than coordinative — the ritual-purity and duty-allocation apparatus is read as manufacturing consent for hierarchy rather than solving a genuine coordination problem. Suppression (0.82) is high and authored as a raw structural property: the hierarchy is maintained through community sanction, family violence, economic exclusion, and — historically — direct legal enforcement, independent of extraction's scope-scaling. Accessibility collapse is authored lower (0.35) relative to a mountain because this reading holds that alternatives (secular law, reform movements, conversion, migration) exist and are actively used, even though the corpus's ideological reach makes full escape difficult for those without resources. Resistance is high (0.78) — Dalit rights movements, Ambedkarite politics, and constitutional litigation constitute substantial organized resistance, which is itself evidence against the naturalization claim any orthodox reading would make.
 *
 * DIRECTIONALITY LOGIC:
 *   Brahmin priestly class and temple institutions sit near the full-beneficiary end: they collect status, income, and administrative authority directly from the hierarchy's continued citation and have institutional exit (arbitrage) allowing them to survive a shift in the corpus's formal authority by rebranding as cultural custodians. Dominant caste landholders benefit materially through labor and marriage-market ordering with moderate exit (secular property relations). Dalit communities and inter-caste couples sit at the full-target end: trapped exit options, generational-to-immediate time horizons, and the most severe and directly named textual sanction (untouchability, endogamy enforcement). Shudra laborers, women, and Adivasi communities occupy intermediate-target positions — constrained rather than fully trapped exit, reflecting partial access to migration, education, or legal remedy, but still bearing structural extraction the corpus is read as authorizing.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem is authored as dead: this reading holds that whatever social-ordering function the classical compilers claimed to solve has no legitimate remaining referent, corroborated by a framer (Ambedkar) who held direct standing as a target of the arrangement rather than by beneficiary-class testimony. The mismatch between founding_problem_status=dead and disappearance_verdict=world_rearranges is intentional and is the signal this reading exists to register: the arrangement's ORIGINAL justification is gone, but its material and social effects persist and would visibly reorganize a great deal if withdrawn — this is precisely the zombie/capture pattern the R5 genealogy interview is built to surface, distinguishing 'still serves its stated purpose' from 'still has effects because enforcement persists independent of purpose.'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly do the three readings of the dharmasastra_corpus kernel (abolitionist_rejection, orthodox_literalist, reformist_contextual) locate their disagreement — is it about the text''s original meaning, its current authority, or its separability into salvageable and non-salvageable components?',
    'No empirical resolution mechanism exists for a normative/interpretive dispute of this kind; the disagreement is conceptual and tracks differing premises about whether textual authority survives independent of its historically oppressive application, and whether an ''ethical core'' can be coherently separated from the hierarchy the text actually prescribes.',
    'If the reformist separability claim is sound, this abolitionist reading''s zero-authority premise is too strong and some coordinative content might survive reconstruction; if the abolitionist reading is sound, reformist reinterpretation is itself a mechanism for preserving beneficiary-class authority under a softened label, which the theater_ratio trajectory in this story is authored to reflect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Sibling readings disagree over whether authority is separable from hierarchy or void entirely.').

omega_variable(
    textual_variation_and_regional_practice,
    'Given multiple, sometimes contradictory Dharmasastra texts (Manusmriti, Yajnavalkya Smriti, regional Dharmashastra commentaries) and vast regional variation in actual customary practice, how uniform was ''the corpus'' as a lived extractive structure versus a retrospectively unified target of critique?',
    'Historical and anthropological scholarship comparing textual prescription against documented regional customary practice across periods; comparative study of enforcement intensity across regions and eras.',
    'If practice varied far more than text implies, the extraction metric authored here may overstate uniformity; if enforcement was broadly consistent despite textual plurality, the high extractiveness and suppression scores are well-supported across the claimed national scope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(textual_variation_and_regional_practice, empirical, 'Whether textual plurality undermines treating ''the corpus'' as one uniform extractive mechanism.').

omega_variable(
    post_abolition_beneficiary_shift_plausibility,
    'Would wholesale abandonment of the corpus''s authority actually produce the beneficiary shift to formerly oppressed groups this reading anticipates, or would dominant-caste material power (land, capital, political organization) persist and reconstitute stratification on secular grounds?',
    'Comparative analysis of jurisdictions and periods where religious-textual authority for stratification was formally withdrawn (e.g., post-independence constitutional abolition of untouchability) to observe whether material hierarchy persisted, weakened, or was replaced by new mechanisms.',
    'If material hierarchy persists despite loss of textual authority (as substantial evidence from post-1950 India suggests, given the suppression_requirement measurements not falling to near-zero), the disappearance_verdict''s optimism about world-rearrangement should be tempered — the corpus may be one enforcement layer among several, not the sole load-bearing structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_abolition_beneficiary_shift_plausibility, empirical, 'Whether removing textual authority alone would suffice to dismantle the material hierarchy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__abolitionist_rejection, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dhar_tr_t0, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(dhar_tr_t0, observed).
narrative_ontology:measurement(dhar_tr_t15, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 15, 0.25).
narrative_ontology:measurement_basis(dhar_tr_t15, observed).
narrative_ontology:measurement(dhar_tr_t30, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 30, 0.32).
narrative_ontology:measurement_basis(dhar_tr_t30, observed).
narrative_ontology:measurement(dhar_tr_t45, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 45, 0.36).
narrative_ontology:measurement_basis(dhar_tr_t45, observed).
narrative_ontology:measurement(dhar_tr_t60, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 60, 0.38).
narrative_ontology:measurement_basis(dhar_tr_t60, observed).
narrative_ontology:measurement(dhar_tr_t75, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 75, 0.4).
narrative_ontology:measurement_basis(dhar_tr_t75, observed).

% Extraction over time
narrative_ontology:measurement(dhar_be_t0, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 0, 0.9).
narrative_ontology:measurement_basis(dhar_be_t0, observed).
narrative_ontology:measurement(dhar_be_t15, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 15, 0.88).
narrative_ontology:measurement_basis(dhar_be_t15, observed).
narrative_ontology:measurement(dhar_be_t30, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 30, 0.83).
narrative_ontology:measurement_basis(dhar_be_t30, observed).
narrative_ontology:measurement(dhar_be_t45, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 45, 0.79).
narrative_ontology:measurement_basis(dhar_be_t45, observed).
narrative_ontology:measurement(dhar_be_t60, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 60, 0.82).
narrative_ontology:measurement_basis(dhar_be_t60, observed).
narrative_ontology:measurement(dhar_be_t75, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 75, 0.87).
narrative_ontology:measurement_basis(dhar_be_t75, observed).

% Suppression requirement over time
narrative_ontology:measurement(dhar_su_t0, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 0, 0.9).
narrative_ontology:measurement_basis(dhar_su_t0, observed).
narrative_ontology:measurement(dhar_su_t15, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 15, 0.85).
narrative_ontology:measurement_basis(dhar_su_t15, observed).
narrative_ontology:measurement(dhar_su_t30, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 30, 0.75).
narrative_ontology:measurement_basis(dhar_su_t30, observed).
narrative_ontology:measurement(dhar_su_t45, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 45, 0.7).
narrative_ontology:measurement_basis(dhar_su_t45, observed).
narrative_ontology:measurement(dhar_su_t60, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 60, 0.78).
narrative_ontology:measurement_basis(dhar_su_t60, observed).
narrative_ontology:measurement(dhar_su_t75, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 75, 0.82).
narrative_ontology:measurement_basis(dhar_su_t75, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dharmasastra_corpus__abolitionist_rejection, identity_coordination).
narrative_ontology:affects_constraint(dharmasastra_corpus__abolitionist_rejection, dharmasastra_corpus__orthodox_literalist).
narrative_ontology:affects_constraint(dharmasastra_corpus__abolitionist_rejection, dharmasastra_corpus__reformist_contextual).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling stories decomposing the natural-language label 'the Dharmasastra corpus' per the ε-invariance principle: dharmasastra_corpus__orthodox_literalist (claims eternal revealed authority; expect low authored suppression-as-illegitimate since the reading treats hierarchy as legitimate and coordinative), dharmasastra_corpus__reformist_contextual (claims separable ethical core; expect intermediate extraction, tangled_rope-flavored), and this abolitionist_rejection reading (zero authority, snare-flavored, highest extraction). Each authors its own ε from its own premises about the standing arrangement; none averages over the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
