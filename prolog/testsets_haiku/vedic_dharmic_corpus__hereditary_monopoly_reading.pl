% ============================================================================
% CONSTRAINT STORY: vedic_dharmic_corpus__hereditary_monopoly_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_dharmic_corpus__hereditary_monopoly_reading, []).

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
 *   constraint_id: vedic_dharmic_corpus__hereditary_monopoly_reading
 *   human_readable: Vedic Dharmic Corpus — Hereditary Monopoly Reading
 *   domain: religious/political/social
 *
 * SUMMARY:
 *   The hereditary-monopoly reading of the Vedic dharmic corpus asserts that
 *   ritual and interpretive authority derive exclusively from birth into the
 *   Brahmin varna, that this hierarchy is divinely ordained in the Vedas, and
 *   that the ritual-purity doctrine justifies excluding lower castes, women,
 *   and untouchables from sacred knowledge and priesthood. This reading
 *   instantiates a high-extraction, actively enforced constraint: Brahmins
 *   benefit materially and in social status from monopolizing ritual
 *   performance and textual interpretation; lower castes and women pay
 *   through ritual fees, barred access, and spiritual mediation dependency;
 *   the constraint persists because temples control ritual access and
 *   Brahminical scholarly tradition controls textual authority. This is ONE
 *   reading of a contested kernel (the Vedic dharmic corpus itself); sibling
 *   readings (bhakti devotional access, reformist egalitarian
 *   reinterpretation) offer competing legitimacy claims that coexist
 *   institutionally and represent organized alternatives to Brahminical
 *   monopoly.
 *
 * KEY AGENTS:
 *   - brahmin_priestly_class: institutional beneficiary and agenda-setter — monopolizes ritual authority and textual interpretation through hereditary lineage and temple control.
 *   - lower_castes: powerless victims — trapped by caste endogamy and geographic immobility; must commission Brahmin priests; barred from Vedic study and independent ritual.
 *   - women: powerless victims — excluded entirely from Vedic study and ritual leadership; identity-locked through kinship and marriage; dependent on male-mediated ritual access.
 *   - untouchables: powerless victims — structurally barred from ritual participation and temple entry; face the highest extraction cost and strongest suppression.
 *   - temple_institution: institutional beneficiary — accumulates wealth and land through ritual-fee economy; enforces the constraint through institutional control.
 *   - bhakti_religious_movement: organized excluded alternative — demonstrates direct devotional access without caste requirement; erodes hereditary monopoly's functional legitimacy.
 *   - reformist_critique_movement: organized excluded alternative — argues caste is historical accretion, not scriptural; advocates constitutional equality and alternative readings.
 *   - colonial_and_postcolonial_authority: institutional observer and partial enforcer — introduces legal contradictions with the hereditary-monopoly reading; enforces constitutional non-discrimination.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.68).
domain_priors:suppression_score(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.72).
domain_priors:theater_ratio(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_dharmic_corpus__hereditary_monopoly_reading, tangled_rope).
narrative_ontology:human_readable(vedic_dharmic_corpus__hereditary_monopoly_reading, "Vedic Dharmic Corpus — Hereditary Monopoly Reading").
narrative_ontology:topic_domain(vedic_dharmic_corpus__hereditary_monopoly_reading, "religious/political/social").

domain_priors:requires_active_enforcement(vedic_dharmic_corpus__hereditary_monopoly_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_dharmic_corpus__hereditary_monopoly_reading, 'ba6e2e25-c618-4ba4-9a1f-10c0273a8e2d').
narrative_ontology:cs_kernel_codification('ba6e2e25-c618-4ba4-9a1f-10c0273a8e2d', fixed_text).
narrative_ontology:cs_authority_grounding('ba6e2e25-c618-4ba4-9a1f-10c0273a8e2d', lineage).
narrative_ontology:cs_interpretation_layer_present('ba6e2e25-c618-4ba4-9a1f-10c0273a8e2d').
narrative_ontology:cs_reading_relation('ba6e2e25-c618-4ba4-9a1f-10c0273a8e2d', vedic_dharmic_corpus__bhakti_devotional_reading, coexists_with).
narrative_ontology:cs_reading_relation('ba6e2e25-c618-4ba4-9a1f-10c0273a8e2d', vedic_dharmic_corpus__reformist_egalitarian_reading, coexists_with).
narrative_ontology:cs_axiom('ba6e2e25-c618-4ba4-9a1f-10c0273a8e2d', foundational, brahmin_birth_ensures_ritual_validity).
narrative_ontology:cs_axiom_status(brahmin_birth_ensures_ritual_validity, holdable).
narrative_ontology:cs_axiom_grounding('ba6e2e25-c618-4ba4-9a1f-10c0273a8e2d', brahmin_birth_ensures_ritual_validity, empirically_contingent).
narrative_ontology:cs_axiom('ba6e2e25-c618-4ba4-9a1f-10c0273a8e2d', foundational, varna_hierarchy_divinely_ordained).
narrative_ontology:cs_axiom_status(varna_hierarchy_divinely_ordained, holdable).
narrative_ontology:cs_axiom_grounding('ba6e2e25-c618-4ba4-9a1f-10c0273a8e2d', varna_hierarchy_divinely_ordained, deontological).
narrative_ontology:cs_reference_frame('ba6e2e25-c618-4ba4-9a1f-10c0273a8e2d', vedic_ritual_authority_brahminically_constituted).
narrative_ontology:cs_drift_state('ba6e2e25-c618-4ba4-9a1f-10c0273a8e2d', postcolonial_constitutional_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ba6e2e25-c618-4ba4-9a1f-10c0273a8e2d', '').
narrative_ontology:cs_kernel_id(vedic_dharmic_corpus__hereditary_monopoly_reading, vedic_dharmic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__hereditary_monopoly_reading, brahmin_priestly_class).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, lower_castes).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, women).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, untouchables).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__hereditary_monopoly_reading, temple_institution).
narrative_ontology:constraint_vindicates(vedic_dharmic_corpus__hereditary_monopoly_reading, varna_hierarchy_divinely_ordained).
narrative_ontology:constraint_vindicates(vedic_dharmic_corpus__hereditary_monopoly_reading, ritual_purity_doctrine).
narrative_ontology:constraint_vindicates(vedic_dharmic_corpus__hereditary_monopoly_reading, brahminical_interpretive_monopoly).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Monopolizes ritual performance, Vedic interpretation, and access to sacred knowledge through hereditary lineage. Controls temple economies, oversees rites of passage, and adjudicates textual meaning. Birth into Brahmin varna is the sole criterion for these roles; the reading asserts this arrangement is divinely ordained in the Vedas and Dharma Shastras. Extracts material benefit (fees, land grants, social deference) from performing rituals for lower castes seeking spiritual efficacy.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, brahmin_priestly_class, agenda_setter,
    institutional, generational, identity_locked, regional).

% Must commission Brahmin priests for rituals deemed essential to spiritual progress and social legitimacy (marriage, death rites, temple offerings, vows). Barred from learning Vedas directly, from performing rituals independently, and from interpreting sacred texts. Pay material fees for priestly services and bear the cost of ritual exclusivity. Geographic immobility and caste endogamy (enforced by community and kinship systems) trap exit options.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, lower_castes, payer,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__hereditary_monopoly_reading, lower_castes, excluded).

% Excluded from Vedic study, ritual leadership, and interpretive authority entirely — the reading declares women ineligible for Brahmin status and Vedic knowledge on textual grounds. Dependent on male relatives (father, husband, son) to commission rituals on their behalf. Bear the spiritual and social cost of mediated access. Identity-lock is enforced through marriage, kinship obligation, and the claim that female participation would pollute sacred space.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, women, payer,
    powerless, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__hereditary_monopoly_reading, women, excluded).

% Structurally barred from ritual participation, temple entry, and priesthood. Pay the highest extraction cost: forced to perform polluting occupations, subject to physical exclusion from sacred spaces, and denied spiritual agency. The reading's framework treats untouchability as divinely ordained and ritually necessary for Brahmin purity maintenance. Geographic and occupational immobility reinforce exit traps.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, untouchables, payer,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__hereditary_monopoly_reading, untouchables, excluded).

% Benefits from the ritual-fee economy and Brahminical control. Temples accumulate land, wealth, and political influence through the constraint's enforcement. The institution is administered by Brahmins but operates as a distinct beneficiary seat: it sustains the extractive system and is sustained by it, though its interests can diverge from individual Brahmin interests during institutional reform or political transition.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, temple_institution, beneficiary,
    institutional, generational, constrained, regional).

% A competing interpretive framework asserting direct devotional access to the divine, bypassing caste requirements and priestly mediation. Bhakti poets and saints (some low-caste, some female) perform rituals and teach independently. They are excluded from the Vedic-hereditary-monopoly framework but represent an organized alternative that erodes the constraint's legitimacy by demonstrating spiritual efficacy outside Brahminical channels.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, bhakti_religious_movement, excluded,
    organized, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__hereditary_monopoly_reading, bhakti_religious_movement, observer).

% Intellectuals, activists, and reform-minded Brahmins argue that caste hierarchy is historical accretion, not scriptural essence, and that constitutional equality principles supersede traditional authority. They produce alternative textual readings, establish reformist institutions, and advocate legal prohibition of caste-based exclusion. They are excluded from the hereditary monopoly framework's legitimacy claims but represent organized contestation.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, reformist_critique_movement, excluded,
    organized, generational, arbitrage, regional).

% Colonial administration initially instrumentalized caste hierarchies but gradually introduced legal frameworks and educational systems that undermined Brahminical textual monopoly. Post-independence, constitutional law declared caste discrimination unlawful, creating a formal legal contradiction with the hereditary-monopoly reading. Enforces the constitutional framework; can override temple autonomy and enforce ritual access rights.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, colonial_and_postcolonial_authority, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_dharmic_corpus__hereditary_monopoly_reading, brahmin_priestly_class).
narrative_ontology:fixing_cost_class(vedic_dharmic_corpus__hereditary_monopoly_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Vedic ritual performed by trained specialists solves a coordination problem: standardized performance of life-cycle rites, maintenance of sacred calendars, and transmission of ritual knowledge across generations. A distributed, non-hereditary priesthood could theoretically solve this. The hereditary-monopoly reading asserts that only birth-ordained Brahmins can perform valid rituals without polluting the sacred order — this is the coordination justification for exclusivity.
% TRANSFER_FUNCTION: Material and status transfers from lower castes, women, and untouchables to the Brahmin priestly class and temple institutions: ritual fees, land grants, labor services, social deference, and monopoly access to spiritual authority. The reading frames these transfers as voluntary compensation for sacred knowledge and ritual efficacy; critics frame them as enforced extraction justified by a constructed doctrine of ritual purity.
% ABSENT_VOICES: Untouchables were systematically excluded from any voice in interpreting texts or defending the hierarchy; their resistance and counter-narratives exist in oral tradition and protest but are absent from the authoritative textual record that the reading monopolizes. Women's interpretations of the Vedas and Dharma Shastras are nearly absent from recorded brahminical scholarship. Bhakti poets and reformist critics challenge the reading from organized alternative positions but are treated as heterodox by hereditary-monopoly adherents.
% DISAPPEARANCE_RATIONALE: If the hereditary-monopoly reading disappeared overnight — if Brahmins lost monopoly control of ritual authority and anyone could perform rites or interpret texts — ritual practice would reorganize: Bhakti practitioners, reformists, and lower-caste leaders would perform rituals immediately; temples would fragment into competing jurisdictions; spiritual authority would distribute across multiple sources. The material economy (temple wealth, priestly income) would shift. The social hierarchy's legitimacy would collapse.
% FOUNDING_PROBLEM: Early Vedic society required standardized ritual performance to maintain cosmic order (rta) and social cohesion; specialized knowledge of Vedic chants, astronomical calculation, and sacrificial procedure was genuinely scarce and required long study. The hereditary-monopoly reading asserts that Brahmin birth ensures ritual purity and interpretive correctness — that caste hierarchy is the divinely prescribed solution to the coordination problem of ritual authority.
% FOUNDING_PROBLEM_CORROBORATION: Brahminical scholars and temple authorities attest the founding problem remains live: ritual performance requires specialized knowledge and ritual purity, both ensured by hereditary Brahmin status. Bhakti practitioners, reformist thinkers, and colonial-era anthropologists attest the founding problem is substantially solved by mechanisms other than caste — devotional efficacy, rational study, and non-hereditary specialization demonstrate that ritual coordination does not require Brahminical monopoly. Constitutional law and modern empirical evidence (rituals performed successfully by non-Brahmins) support the contested assessment.
narrative_ontology:disappearance_verdict(vedic_dharmic_corpus__hereditary_monopoly_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_dharmic_corpus__hereditary_monopoly_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_dharmic_corpus__hereditary_monopoly_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(vedic_dharmic_corpus__hereditary_monopoly_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_dharmic_corpus__hereditary_monopoly_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vedic_dharmic_corpus__hereditary_monopoly_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vedic_dharmic_corpus__hereditary_monopoly_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) and rising (from 0.55 at start) because the constraint transfers material resources (ritual fees, land grants, labor) from lower castes to Brahmins without justification by service cost — the 'coordination' function (standardized rituals) could be performed by trained non-Brahmins, which exposes the extraction as monopoly rent. Suppression is also high (0.72) and rising because maintaining the monopoly requires active enforcement: occupational restriction (non-Brahmins barred from priesthood), educational exclusion (Vedic study prohibited), temple access control (lower castes and women restricted from sacred spaces). Theater ratio rises (from 0.22 to 0.41) because as bhakti and reformist alternatives gain institutional presence, Brahminical defenders increasingly invest in performative maintenance of the legitimacy claim — elaborate ritual justifications, theological elaborations, symbolic authority performances — rather than addressing the functional challenge that non-Brahmins can perform valid rituals. Accessibility collapse is high (0.78) because once a person is born into a lower caste or female, exit from the constraint is nearly complete: occupational mobility is blocked, geographic mobility is constrained by community enforcement, and identity-reconstruction (claiming Brahmin status) is impossible. Resistance is moderate-high (0.64) because organized alternatives (Bhakti, reformism) mount continuous institutional and intellectual challenge, but suppression and caste-endogamy-enforcement prevent majority defection. The measurements are authored on one shared time grid covering an interval of 20 (representing roughly the last 2–3 centuries of recorded Hindu social dynamics, from pre-colonial through late-colonial to early post-independence — the period when bhakti and reformist alternatives became organized and extractiveness became more visible).
 *
 * PERSPECTIVAL GAP:
 *   From the Brahmin priestly seat, the constraint is a genuine coordination mechanism: Vedic ritual requires specialized knowledge, ritual purity must be maintained, and hereditary training ensures both. The constraint is experienced as obligation (dharma) and divine duty. From the lower-caste seat, the same structure is enforced extraction: ritual coordination could work without caste monopoly (as bhakti demonstrates), and the purity doctrine is a justification for collecting rents. From the female seat, the constraint is doubly extractive: excluded from even the coordinating function, dependent on male relatives for spiritual mediation, and told that the exclusion is divine. The engine computes these seat-specific types from the structural data; the claim and metrics remain independent — the authored claim (tangled rope) reflects the Brahmin framing (it coordinates ritual, it requires enforcement, there are beneficiaries and victims), but the metrics (high extraction, high suppression, rising theater) describe how extractive and defensive it looks from the payer seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Brahmin priests (institutional power, high time-horizon, identity-locked exit) sit near d=0.0 (full beneficiary): they collect the extraction, control the agenda, and cannot exit the arrangement without losing their identity and material position. Lower castes and women (powerless, long time horizons, trapped/identity-locked exit) sit near d=1.0 (full target): they bear the cost, cannot change the rules, and cannot exit. Untouchables sit at the extreme (d approaching 1.0) because suppression is maximal and alternatives are fewest. Temple institutions sit near d=0.2-0.3 (beneficiary but not setter): they benefit materially but depend on Brahminical administration; they could be reformed but defensive institutional interests keep them aligned with the constraint. Bhakti and reformist movements sit near d=0.5-0.6 (contested/symmetric): they generate counter-extraction pressure and have growing institutional bases, but they are still excluded from textual authority and do not yet control temple hierarchies. The engine derives these directionalities from the beneficiary/victim declarations and exit-option atoms; the commentary explains why the same constraint produces radically different d-values across seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was genuine: Vedic ritual required specialized knowledge and standardized performance. But the hereditary-monopoly reading asserts that ONLY Brahmin birth can ensure ritual validity — a claim that becomes indefensible once Bhakti practitioners demonstrate spiritual efficacy outside the monopoly and reformists produce alternative textual readings. The constraint persists not because the founding problem is unsolved but because Brahminical institutional control prevents alternatives from displacing the hereditary monopoly even as the legitimacy claim deteriorates. This is the core mandatrophy signature: the arrangement's mandate (ensure valid ritual) is no longer bound to the Brahminical monopoly (alternatives demonstrate validity), but the arrangement persists through enforced institutional monopoly. Declaring mandatrophy_resolved here means the analysis recognizes that the founding problem admits non-Brahminical solutions — caste hierarchy is not structurally necessary to ritual coordination, it is extractively defended. The theater-ratio rise (from 0.22 to 0.41) supports this: increasing share of Brahminical effort goes to defending legitimacy claims rather than performing unique coordination functions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_contest_hereditary_vs_devotional,
    'Does the Vedic corpus logically foreclose bhakti devotional access to the divine, or can both framings coexist as valid interpretations of the same textual corpus?',
    'Textual hermeneutics: do foundational passages in the Vedas and Upanishads that emphasize direct knowledge (jnana) and devotion (bhakti) logically contradict passages emphasizing caste duty (varna-vichara)? Or can a single reading accommodate both by treating them as context-dependent or as addressing different audiences?',
    'If logically foreclosed: the hereditary-monopoly and bhakti readings cannot coexist in a single coherent framework — one reading is structurally false. If coexisting: both readings remain live options held by different parties, which changes the nature of the contest from logical refutation to institutional power struggle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_hereditary_vs_devotional, conceptual, 'Whether hereditary-monopoly and bhakti readings logically foreclose each other or coexist as multiple valid interpretations.').

omega_variable(
    textual_foundation_vs_historical_accretion,
    'Is the varna hierarchy fundamentally prescribed in the Vedic corpus, or is it a historical accretion layered onto egalitarian or ambiguous textual foundations?',
    'Philological and historical analysis: comparative study of Vedic texts across layers (Rg Veda, Yajur Veda, Upanishads, Dharma Shastras) to map when and how caste-hierarchy language emerges; examination of pre-Vedic social organization and textual variants. The reformist reading claims caste hierarchy emerges late and is not foundational.',
    'If historically accreted: the constraint loses textual legitimacy and becomes a power grab defended by invented doctrine — the claim that varna is ''divinely ordained'' collapses. If foundational: the hereditary-monopoly reading''s core legitimacy claim is validated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_foundation_vs_historical_accretion, empirical, 'Whether caste hierarchy is Vedic-foundational or historically accreted doctrine.').

omega_variable(
    ritual_efficacy_source,
    'Is ritual efficacy (the spiritual power of correctly performed rites) contingent on the performer''s caste status, or is it contingent on knowledge, intention, and procedure alone?',
    'Empirical observation: do rituals performed by non-Brahmin practitioners produce the same spiritual/social effects as those performed by Brahmins? Textual analysis of ritual efficacy theories in the Vedas — does the corpus assert caste-contingent efficacy or knowledge-contingent efficacy?',
    'If caste-contingent: the hereditary monopoly is structurally necessary to guarantee valid ritual. If knowledge-contingent: non-Brahmins with training could perform valid rituals, which dismantles the monopoly''s functional justification and exposes it as pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ritual_efficacy_source, empirical, 'Whether ritual efficacy is caste-dependent or knowledge-dependent.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.72) enforced structurally through legal/institutional barriers (occupation restriction, temple exclusion, education prohibition) or internalized through religious belief (the lower-caste subject believes their exclusion is divinely ordained and spiritually necessary)?',
    'Post-enforcement observation: in contexts where legal barriers are removed or relaxed, do suppression and compliance persist at previous levels? Do lower-caste individuals who have access to alternative spiritual frameworks (Bhakti, reform, secular education) still accept the hereditary-monopoly reading''s legitimacy?',
    'If structural: removing institutional enforcement (changing temple law, eliminating occupation caste-linking) would reduce suppression quickly. If internalized: suppression persists even after external barriers fall, requiring re-education and identity reconstruction. If both: the constraint is doubly resistant to reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural or internalized in the victims'' consciousness.').

omega_variable(
    female_exclusion_foundational_vs_instrumental,
    'Does the Vedic corpus mandatorily exclude women from Vedic study and ritual performance on foundational theological grounds, or is female exclusion an instrumental application of a gender-separate-roles doctrine that could be reinterpreted?',
    'Textual analysis of passages addressing women (Upanishadic references to female sages, Rig Vedic invocations). Historical evidence of female ritual specialists (Brahmavadinis, female ascetics). Comparative analysis with sibling readings'' treatment of female spiritual authority.',
    'If foundational: women are doubly victimized and the reformist reading must reject the Vedic corpus itself. If instrumental: women''s exclusion could be reformed within the textual framework, which changes the political economy of the constraint — female Brahmins could emerge and create internal fracture in the monopoly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(female_exclusion_foundational_vs_instrumental, empirical, 'Whether female exclusion is Vedically foundational or historically layered.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_dharmic_corpus__hereditary_monopoly_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t0, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(vedi_tr_t0, observed).
narrative_ontology:measurement(vedi_tr_t5, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 5, 0.27).
narrative_ontology:measurement_basis(vedi_tr_t5, observed).
narrative_ontology:measurement(vedi_tr_t10, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(vedi_tr_t10, observed).
narrative_ontology:measurement(vedi_tr_t15, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement_basis(vedi_tr_t15, observed).
narrative_ontology:measurement(vedi_tr_t20, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(vedi_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(vedi_be_t0, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(vedi_be_t0, observed).
narrative_ontology:measurement(vedi_be_t5, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement_basis(vedi_be_t5, observed).
narrative_ontology:measurement(vedi_be_t10, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement_basis(vedi_be_t10, observed).
narrative_ontology:measurement(vedi_be_t15, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement_basis(vedi_be_t15, observed).
narrative_ontology:measurement(vedi_be_t20, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement_basis(vedi_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t0, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(vedi_su_t0, observed).
narrative_ontology:measurement(vedi_su_t5, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(vedi_su_t5, observed).
narrative_ontology:measurement(vedi_su_t10, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 10, 0.67).
narrative_ontology:measurement_basis(vedi_su_t10, observed).
narrative_ontology:measurement(vedi_su_t15, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement_basis(vedi_su_t15, observed).
narrative_ontology:measurement(vedi_su_t20, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement_basis(vedi_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_dharmic_corpus__hereditary_monopoly_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.12).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__hereditary_monopoly_reading, vedic_dharmic_corpus__bhakti_devotional_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__hereditary_monopoly_reading, vedic_dharmic_corpus__reformist_egalitarian_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__hereditary_monopoly_reading, temple_ritual_economy_constraint).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__hereditary_monopoly_reading, caste_based_occupational_restriction).

% DUAL FORMULATION NOTE:
% The Vedic dharmic corpus is a contested kernel instantiated in three constraint stories: hereditary_monopoly_reading (this file) asserts caste hierarchy is divinely ordained; bhakti_devotional_reading asserts direct devotional access bypasses caste requirement; reformist_egalitarian_reading asserts caste hierarchy is historical accretion. Each reading has distinct epsilon, beneficiary/victim structure, and institutional bases. They are linked as coexisting alternative interpretations of the same textual corpus, not as sequential stages. The hereditary-monopoly reading's epsilon (~0.68) reflects measured extraction; the bhakti and reformist readings instantiate lower epsilons (higher coordination, lower pure extraction) because they describe arrangements where multiple legitimacy sources coexist and Brahminical monopoly is not enforced. Decomposition follows DP-001 (epsilon-invariance): measuring 'the Vedic dharmic corpus constraint' via different readings yields different epsilon values, which means it is actually three constraints with shared kernel/authority but distinct extraction profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vedic_dharmic_corpus__hereditary_monopoly_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
