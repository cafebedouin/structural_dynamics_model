% ============================================================================
% CONSTRAINT STORY: waitangi_sovereignty_allocation__crown_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_waitangi_sovereignty_allocation__crown_sovereignty_reading, []).

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
 *   constraint_id: waitangi_sovereignty_allocation__crown_sovereignty_reading
 *   human_readable: Crown Sovereignty Reading of the Treaty of Waitangi (English Article I / Westminster Supremacy)
 *   domain: constitutional_law/indigenous_rights/post_colonial_governance
 *
 * SUMMARY:
 *   This story authors the Crown sovereignty reading of the Treaty of
 *   Waitangi kernel: the constitutional position that the English-language
 *   Article I effected a complete and unilateral cession of sovereignty to
 *   the Crown, establishing Westminster parliamentary supremacy as the sole
 *   operative source of legal authority in Aotearoa/New Zealand, with no
 *   structural requirement for ongoing Māori consent to legislative or
 *   resource-allocation decisions. This reading has historically grounded
 *   confiscation legislation (e.g. the New Zealand Settlements Act 1863), the
 *   Native/Māori Land Court's individualization of communal title, and
 *   resource legislation extinguishing customary rights. It is ONE of three
 *   constitutionally live readings of the same treaty text; the
 *   partnership_reading and rangatiratanga_reading are separate constraint
 *   stories with their own ε and stakeholder structures, linked here via
 *   network.affects_constraints. The extraction and suppression this story
 *   authors peaked during the confiscation era (1860s-1900s) and have
 *   declined, though not disappeared, as the Waitangi Tribunal process and
 *   later jurisprudence (e.g. partnership-inflected case law) have partially
 *   constrained the reading's unilateral application without displacing it as
 *   formal constitutional doctrine.
 *
 * KEY AGENTS:
 *   - the_crown: agenda_setter (institutional/arbitrage) — administers and enforces the sovereignty claim
 *   - settler_colonial_administration: beneficiary (institutional/mobile) — gains administrative and legal certainty
 *   - post_treaty_land_purchasers: beneficiary (powerful/mobile) — acquires land whose title depends on the reading holding
 *   - iwi_and_hapu: payer (organized/trapped) — political authority treated as absorbed without consent
 *   - maori_land_owners: payer (powerless/trapped) — bears direct land loss under the reading's legal machinery
 *   - maori_fishing_and_resource_communities: payer (organized/constrained) — customary rights treated as extinguishable
 *   - westminster_parliament_and_courts: observer/agenda_setter (institutional/analytical) — both guarantor and potential reviser of the doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.81).
domain_priors:suppression_score(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.78).
domain_priors:theater_ratio(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__crown_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__crown_sovereignty_reading, "Crown Sovereignty Reading of the Treaty of Waitangi (English Article I / Westminster Supremacy)").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__crown_sovereignty_reading, "constitutional_law/indigenous_rights/post_colonial_governance").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__crown_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__crown_sovereignty_reading, 'f9b9c25e-b612-4fb6-be7c-f25736937a67').
narrative_ontology:cs_kernel_codification('f9b9c25e-b612-4fb6-be7c-f25736937a67', fixed_text).
narrative_ontology:cs_authority_grounding('f9b9c25e-b612-4fb6-be7c-f25736937a67', extraction).
narrative_ontology:cs_interpretation_layer_present('f9b9c25e-b612-4fb6-be7c-f25736937a67').
narrative_ontology:cs_reading_relation('f9b9c25e-b612-4fb6-be7c-f25736937a67', waitangi_sovereignty_allocation__partnership_reading, influences).
narrative_ontology:cs_reading_relation('f9b9c25e-b612-4fb6-be7c-f25736937a67', waitangi_sovereignty_allocation__rangatiratanga_reading, forecloses).
narrative_ontology:cs_axiom('f9b9c25e-b612-4fb6-be7c-f25736937a67', foundational, english_text_is_sole_controlling_instrument).
narrative_ontology:cs_axiom_status(english_text_is_sole_controlling_instrument, holdable).
narrative_ontology:cs_axiom_grounding('f9b9c25e-b612-4fb6-be7c-f25736937a67', english_text_is_sole_controlling_instrument, conventional).
narrative_ontology:cs_axiom('f9b9c25e-b612-4fb6-be7c-f25736937a67', foundational, sovereignty_cession_requires_no_ongoing_consent).
narrative_ontology:cs_axiom_status(sovereignty_cession_requires_no_ongoing_consent, holdable).
narrative_ontology:cs_axiom_grounding('f9b9c25e-b612-4fb6-be7c-f25736937a67', sovereignty_cession_requires_no_ongoing_consent, conventional).
narrative_ontology:cs_reference_frame('f9b9c25e-b612-4fb6-be7c-f25736937a67', unqualified_westminster_supremacy).
narrative_ontology:cs_drift_state('f9b9c25e-b612-4fb6-be7c-f25736937a67', contemporary_tribunal_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f9b9c25e-b612-4fb6-be7c-f25736937a67', '').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__crown_sovereignty_reading, the_crown).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__crown_sovereignty_reading, settler_colonial_administration).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__crown_sovereignty_reading, post_treaty_land_purchasers).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__crown_sovereignty_reading, iwi_and_hapu).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_land_owners).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_fishing_and_resource_communities).
narrative_ontology:constraint_vindicates(waitangi_sovereignty_allocation__crown_sovereignty_reading, parliamentary_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(waitangi_sovereignty_allocation__crown_sovereignty_reading, westminster_constitutional_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Reads the English-language Article I as a complete cession of sovereignty, and on that basis exercises plenary legislative power over land, resources, and governance without requiring Māori consent. It sets courts, enacts confiscation and native land legislation, and administers the machinery that enforces this reading against competing textual claims.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, the_crown, agenda_setter,
    institutional, generational, arbitrage, national).

% Colonial and later settler governments administer land courts, surveys, and purchase schemes premised on the Crown's plenary sovereignty; they gain legal certainty and administrative control over territory and resources that the Crown reading makes available without a Māori veto.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, settler_colonial_administration, beneficiary,
    institutional, generational, mobile, national).

% Settlers and land companies acquire land through Crown-sanctioned purchase and confiscation processes that depend on the Crown's sovereignty claim overriding rangatiratanga; their title security rests entirely on the Crown reading holding as the operative constitutional fact.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, post_treaty_land_purchasers, beneficiary,
    powerful, biographical, mobile, regional).

% Iwi and hapū signed the Māori-language text promising retained rangatiratanga; under the Crown sovereignty reading their political authority is treated as fully absorbed into Crown legislative power. They cannot exit the jurisdiction the reading imposes; their recourse is litigation, petition, and political organizing within a system whose founding premise denies their consent was ever structurally required.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, iwi_and_hapu, payer,
    organized, civilizational, trapped, national).

% Individual and whānau landholders lose land through Native Land Court processes and confiscation statutes that are legally coherent only if the Crown's plenary sovereignty claim is accepted; they bear the direct material transfer the reading authorizes and have no forum within the reading's own logic to contest the underlying allocation of sovereignty itself.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_land_owners, payer,
    powerless, generational, trapped, regional).

% Communities whose customary fisheries and resource rights are treated as extinguishable by Crown legislation under the sovereignty reading, since kāwanatanga-only limits on Crown power are not recognized as binding; can pursue Waitangi Tribunal claims but those claims proceed under authority the Crown itself grants and can define.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_fishing_and_resource_communities, payer,
    organized, generational, constrained, regional).

% Parliament and the judiciary apply and periodically revisit the doctrine of parliamentary sovereignty as the operative constitutional principle; they both interpret the Crown reading's implications and have the formal power to modify how far it extends, making them simultaneously the reading's guarantors and its potential revisers.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, westminster_parliament_and_courts, observer,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__crown_sovereignty_reading, westminster_parliament_and_courts, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(waitangi_sovereignty_allocation__crown_sovereignty_reading, the_crown).
narrative_ontology:fixing_cost_class(waitangi_sovereignty_allocation__crown_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, determinate locus of legislative authority (Westminster-style parliamentary supremacy) so that colonial and later national governance has one unambiguous source of law rather than contested or overlapping jurisdictions.
% TRANSFER_FUNCTION: Moves political authority, land, and resource control from iwi and hapū to the Crown and, through Crown-sanctioned purchase and confiscation, onward to settlers and the colonial state — justified by treating the English text's cession of sovereignty as the sole operative fact.
% ABSENT_VOICES: The Māori-language signatories, whose text (Article II, rangatiratanga) was not preserved as controlling in this reading, are structurally absent from the reading's own interpretive frame: their understanding of what was ceded is treated as not legally decisive rather than weighed and rejected on the merits.
% DISAPPEARANCE_RATIONALE: If the Crown sovereignty reading were displaced as the operative constitutional premise, legislative acts, land title chains, and resource allocations built on unilateral Crown authority since 1840 would lose their unquestioned legal foundation, opening land and resource allocation to renegotiation on partnership or rangatiratanga terms and requiring Crown consent mechanisms that do not currently exist.
% FOUNDING_PROBLEM: Colonial administrators needed a single, judicially cognizable source of sovereign authority to found a functioning settler state and legal system in a territory where indigenous political authority was extensive and plural.
% FOUNDING_PROBLEM_CORROBORATION: The Crown and its courts (inside the reading) continue to attest that this cession is settled constitutional fact. The Waitangi Tribunal, a Crown-established but independent inquiry body, and international legal scholarship on treaty interpretation (outside the reading's own beneficiary set) attest that the English text's cession claim is contested and inconsistent with the Māori text and with normal treaty-interpretation principles favoring the indigenous-language version signed by the vast majority of signatories.
narrative_ontology:disappearance_verdict(waitangi_sovereignty_allocation__crown_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(waitangi_sovereignty_allocation__crown_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(waitangi_sovereignty_allocation__crown_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(waitangi_sovereignty_allocation__crown_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(waitangi_sovereignty_allocation__crown_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(waitangi_sovereignty_allocation__crown_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(waitangi_sovereignty_allocation__crown_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.81) at the story's base-properties snapshot because the reading's operation directly authorizes transfer of land, resources, and political authority from Māori communities to the Crown and settler interests without a consent gate — the temporal series shows this extraction peaking during the 1860s-1900s confiscation and land-court era (0.85) and declining but not vanishing by 2024 (0.55) as Tribunal processes and partnership-inflected jurisprudence constrain application. Suppression is authored even higher at peak (0.90 at 1863) reflecting the New Zealand Wars and confiscation legislation's direct coercive enforcement of the sovereignty claim against armed and political resistance; suppression declines as coercive enforcement is replaced by more consultative (though still Crown-controlled) mechanisms. Theater ratio rises over the interval (0.10 to 0.35) as the reading persists more through formal doctrinal maintenance (courts citing parliamentary sovereignty as settled) than through active new extraction — a genuine but partial piton-adjacent drift within an otherwise still-operative tangled rope. Accessibility collapse (0.62) and resistance (0.72) reflect that Māori political and legal resistance to the reading has been continuous and substantial since 1840, never fully suppressed, distinguishing this from a genuine mountain.
 *
 * PERSPECTIVAL GAP:
 *   From the Crown/settler-administration seat, the sovereignty reading functions as necessary constitutional coordination — a single source of law without which governance would be impossible. From the iwi/hapū and Māori landowner seats, the identical structure operates as enforced extraction: political authority and land moved without consent, defended by legislation and, historically, military force. The engine should compute markedly different seat-level classifications from these structurally opposed positions even though both examine the same constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   The Crown and settler administration are declared beneficiaries because they gain legal certainty, land, and unchallenged legislative authority directly from the reading holding as operative fact — low d, near the subsidized end. Iwi/hapū, Māori landowners, and resource communities are declared victims because political authority, land, and customary rights are their assets that the reading treats as extinguishable or absorbed — high d, near the full-target end, amplified by trapped exit options (they cannot leave the jurisdiction the reading constitutes). Westminster parliament and courts sit in an analytical/agenda-setting dual role: they are the doctrine's enforcers but also the only body positioned to revise it, which is why 'observer' is paired with 'agenda_setter' as secondary role rather than treating them as pure beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — establishing a single determinate source of governing authority for a new settler state — was live in 1840 and substantially resolved by the mid-20th century as functioning state institutions matured; the doctrine's persistence past that point, especially its use to foreclose Māori consent requirements on resource allocation decades after the state's basic institutional viability was secure, is a mandatrophy signature: continued application of a founding-era doctrine to contemporary allocation questions it was never actually needed to answer. The founding_problem_status is authored 'contested' rather than 'dead' because the Crown's own institutions still treat the doctrine as live and necessary, while Tribunal findings and comparative treaty jurisprudence treat its founding rationale as resolved — exactly the mismatch the R5 consumer is designed to flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_authority_ambiguity,
    'Does the English-language Article I or the Māori-language Article II (promising rangatiratanga) constitute the legally and morally controlling text of the Treaty?',
    'Comparative treaty-interpretation doctrine (contra proferentem favoring the indigenous signatories'' understanding), Waitangi Tribunal findings, and international law standards for treaties between unequal parties translated across languages.',
    'If the Māori text controls, the Crown sovereignty reading''s foundational premise is invalid ab initio and the constraint this story describes was never legitimately founded, though it has operated as if it were; if the English text controls, the reading''s premise holds and the extraction it authors is not a boundary violation but the reading''s intended operation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_authority_ambiguity, conceptual, 'Which treaty text is controlling — the central textual fact the whole kernel contest turns on.').

omega_variable(
    natural_law_vs_constructed_doctrine,
    'Is Westminster parliamentary supremacy a genuine structural necessity for any functioning modern state (mountain-like), or a constructed doctrine that happens to benefit the Crown and settler interests in this specific application?',
    'Comparative constitutional analysis of federal and treaty-based states (Canada, the US, South Africa) that maintain functioning governance while recognizing constitutionally entrenched indigenous or sub-national sovereignty limits on legislative supremacy.',
    'If comparative cases show functioning states without unconstrained parliamentary supremacy, the doctrine is revealed as one contingent institutional choice among several viable alternatives rather than a necessary feature of governance — undermining the coordination-necessity justification for the reading''s extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_doctrine, empirical, 'Whether parliamentary supremacy is structurally necessary or a contingent, interest-serving institutional choice.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly do the three kernel readings disagree — is it the controlling text, the scope of what ''kāwanatanga'' versus ''sovereignty'' means, or the legal effect of consent obtained under conditions of asymmetric information and translation?',
    'Structural comparison of the three sibling constraint stories'' beneficiary/victim declarations and cs_structure.axioms to identify the precise point of divergence.',
    'Locating the disagreement precisely (translation vs. scope vs. consent-validity) determines which remedial mechanism (retranslation, partnership co-governance, or rangatiratanga restoration) would actually resolve the underlying dispute rather than merely restating it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'This constraint is one reading of the waitangi_sovereignty_allocation kernel; the partnership_reading and rangatiratanga_reading are sibling constraints, and this omega documents that the disagreement is located at the level of controlling text and the meaning of ceded authority, not merely differing evaluations of the same facts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(waitangi_sovereignty_allocation__crown_sovereignty_reading, 1840, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wait_tr_t1840, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 1840, 0.1).
narrative_ontology:measurement(wait_tr_t1863, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 1863, 0.08).
narrative_ontology:measurement(wait_tr_t1900, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 1900, 0.12).
narrative_ontology:measurement(wait_tr_t1940, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 1940, 0.2).
narrative_ontology:measurement(wait_tr_t1975, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 1975, 0.28).
narrative_ontology:measurement(wait_tr_t2000, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 2000, 0.32).
narrative_ontology:measurement(wait_tr_t2024, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 2024, 0.35).

% Extraction over time
narrative_ontology:measurement(wait_be_t1840, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 1840, 0.58).
narrative_ontology:measurement(wait_be_t1863, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 1863, 0.74).
narrative_ontology:measurement(wait_be_t1900, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 1900, 0.85).
narrative_ontology:measurement(wait_be_t1940, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 1940, 0.8).
narrative_ontology:measurement(wait_be_t1975, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 1975, 0.7).
narrative_ontology:measurement(wait_be_t2000, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 2000, 0.63).
narrative_ontology:measurement(wait_be_t2024, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 2024, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(wait_su_t1840, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 1840, 0.55).
narrative_ontology:measurement(wait_su_t1863, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 1863, 0.9).
narrative_ontology:measurement(wait_su_t1900, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 1900, 0.82).
narrative_ontology:measurement(wait_su_t1940, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 1940, 0.7).
narrative_ontology:measurement(wait_su_t1975, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 1975, 0.6).
narrative_ontology:measurement(wait_su_t2000, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 2000, 0.48).
narrative_ontology:measurement(wait_su_t2024, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 2024, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(waitangi_sovereignty_allocation__crown_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.1).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_sovereignty_allocation__partnership_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_sovereignty_allocation__rangatiratanga_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraint stories decomposing the natural-language 'Treaty of Waitangi sovereignty question' per the ε-invariance principle: measuring the treaty's sovereignty allocation by the English text (this story) versus the Māori text (rangatiratanga_reading) versus a good-faith partnership standard (partnership_reading) yields structurally different ε, beneficiary/victim sets, and classifications. They are not the same constraint viewed three ways; they are three constraints sharing a textual kernel, linked here so contamination/coupling analysis can trace how legitimacy pressure on one reading propagates to the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
