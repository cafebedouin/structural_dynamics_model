% ============================================================================
% CONSTRAINT STORY: vedic_dharmic_corpus__hereditary_monopoly_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: vedic_dharmic_corpus__hereditary_monopoly_reading
 *   human_readable: Vedic Dharmic Corpus: Hereditary Brahmin Monopoly Reading
 *   domain: religious/social_stratification/interpretive_legitimacy
 *
 * SUMMARY:
 *   This constraint instantiates the hereditary-monopoly reading of the Vedic
 *   Dharmic Corpus — the interpretation that ritual and interpretive
 *   authority derive exclusively from birth into the Brahmin lineage, that
 *   the varna (caste) hierarchy is divinely ordained in the texts themselves,
 *   and that lower castes and women are rightfully excluded from ritual
 *   performance and Vedic study by the structure of creation itself. The
 *   reading grounds Brahmin priestly authority in an immutable cosmic order
 *   (dharma), making the hierarchy not a social choice but a metaphysical
 *   fact. The constraint extracts substantial economic and social rents:
 *   ritual monopoly concentrates income and prestige, interpretive monopoly
 *   ensures that textual meaning cannot be challenged by lower-caste or women
 *   scholars, and institutional control (temples, initiation rites, funeral
 *   ceremonies) makes exit impossible for those embedded in the Hindu
 *   religious economy. The reading is actively defended: Brahmin institutions
 *   have historically resisted vernacular translation of texts, legal
 *   challenges to caste restriction, and alternative readings that claim
 *   egalitarian intent. At the same time, the reading is clothed in the
 *   language of natural law and cosmic order (theater_ratio 0.42), performing
 *   'discovery' of eternal truth rather than active maintenance of
 *   institutional power.
 *
 * KEY AGENTS:
 *   - brahmin_priestly_class: Monopoly holder on ritual performance and scriptural interpretation; collects economic rents from ritual services and maintains institutional control over legitimate meaning-making.
 *   - lower_castes: Structurally excluded from ritual performance, Vedic study, and interpretive authority; economically dependent on Brahmin-controlled ritual services for life-cycle ceremonies.
 *   - women_of_all_castes: Excluded from Vedic recitation and full ritual participation regardless of birth; historically barred from scriptural learning and interpretive authority.
 *   - vedic_textual_tradition: The kernel — the body of texts whose interpretation grounds authority claims. The hereditary monopoly reading asserts the texts are unambiguous on caste hierarchy; alternative readings dispute this.
 *   - temple_institutional_complex: The enforcement apparatus — owns ritual space, controls access, certifies legitimate ritual practitioners, and channels ritual income to Brahmin controllers.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.65).
domain_priors:suppression_score(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.72).
domain_priors:theater_ratio(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_dharmic_corpus__hereditary_monopoly_reading, tangled_rope).
narrative_ontology:human_readable(vedic_dharmic_corpus__hereditary_monopoly_reading, "Vedic Dharmic Corpus: Hereditary Brahmin Monopoly Reading").
narrative_ontology:topic_domain(vedic_dharmic_corpus__hereditary_monopoly_reading, "religious/social_stratification/interpretive_legitimacy").

domain_priors:requires_active_enforcement(vedic_dharmic_corpus__hereditary_monopoly_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_dharmic_corpus__hereditary_monopoly_reading, 'd9b12a9f-6100-411a-b200-4b1e8fda0e01').
narrative_ontology:cs_kernel_codification('d9b12a9f-6100-411a-b200-4b1e8fda0e01', fixed_text).
narrative_ontology:cs_authority_grounding('d9b12a9f-6100-411a-b200-4b1e8fda0e01', lineage).
narrative_ontology:cs_interpretation_layer_present('d9b12a9f-6100-411a-b200-4b1e8fda0e01').
narrative_ontology:cs_reading_relation('d9b12a9f-6100-411a-b200-4b1e8fda0e01', vedic_dharmic_corpus__bhakti_devotional_reading, coexists_with).
narrative_ontology:cs_reading_relation('d9b12a9f-6100-411a-b200-4b1e8fda0e01', vedic_dharmic_corpus__reformist_egalitarian_reading, influences).
narrative_ontology:cs_axiom('d9b12a9f-6100-411a-b200-4b1e8fda0e01', foundational, brahmin_hereditary_ritual_monopoly).
narrative_ontology:cs_axiom_status(brahmin_hereditary_ritual_monopoly, holdable).
narrative_ontology:cs_axiom_grounding('d9b12a9f-6100-411a-b200-4b1e8fda0e01', brahmin_hereditary_ritual_monopoly, deontological).
narrative_ontology:cs_axiom('d9b12a9f-6100-411a-b200-4b1e8fda0e01', foundational, varna_hierarchy_divinely_ordained).
narrative_ontology:cs_axiom_status(varna_hierarchy_divinely_ordained, holdable).
narrative_ontology:cs_axiom_grounding('d9b12a9f-6100-411a-b200-4b1e8fda0e01', varna_hierarchy_divinely_ordained, empirically_contingent).
narrative_ontology:cs_reference_frame('d9b12a9f-6100-411a-b200-4b1e8fda0e01', vedic_ritual_authority_hereditary_brahmanism).
narrative_ontology:cs_drift_state('d9b12a9f-6100-411a-b200-4b1e8fda0e01', contemporary_constitutional_and_reformist_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d9b12a9f-6100-411a-b200-4b1e8fda0e01', '').
narrative_ontology:cs_kernel_id(vedic_dharmic_corpus__hereditary_monopoly_reading, vedic_dharmic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__hereditary_monopoly_reading, brahmin_priestly_class).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, lower_castes).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, women_of_all_castes).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, excluded_varnas).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls temples, conducts rituals, interprets Vedic texts, certifies legitimate practitioners through initiation, collects ritual fees, and maintains the textual tradition. Claims authority derives from birth into Brahmin lineage and from custodianship of divine truth. Has material and symbolic interest in perpetuating the reading that reserves interpretive authority to Brahmins. Can migrate, exit to urban professions, or adopt alternative readings if institutional power erodes — but maintains the monopoly reading as long as institutional conditions permit.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, brahmin_priestly_class, agenda_setter,
    institutional, civilizational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__hereditary_monopoly_reading, brahmin_priestly_class, beneficiary).

% Excluded by birth from ritual performance and Vedic study. Economically dependent on Brahmin priests for life-cycle rituals (birth, marriage, death ceremonies). Barred from entering temples, reading sacred texts, interpreting dharma. Subject to spiritual narratives that frame their exclusion as cosmic justice (karma from past lives justifies current status). Exit from the constraint requires rejecting the caste-birth-identity framework itself, which is identity-constitutive for agents embedded in the Hindu religious economy — not a realistic option for those whose family identity, marriage prospects, and social standing depend on caste status. Pay through ritual fees, deference, and labor obligation.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, lower_castes, payer,
    powerless, biographical, identity_locked, continental).

% Excluded from Vedic recitation and full ritual participation regardless of caste birth. Even Brahmin women cannot conduct major rituals or interpret texts with authority. Confined to domestic ritual roles (cooking offerings, maintaining household shrine). Subject to narratives that women's ritual incompleteness is cosmic law. Identity-locked by gender, married into new lineages, and dependent on male family members for ritual standing. Pay through restricted life choices, dependency on male interpreters for spiritual matters, and exclusion from authoritative knowledge.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, women_of_all_castes, payer,
    powerless, biographical, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__hereditary_monopoly_reading, women_of_all_castes, excluded).

% Owns temples and ritual spaces, controls who may enter and perform ritual, maintains the initiation chain that certifies Brahmin ritual authority, channels ritual fees and land revenue to Brahmin controllers, employs non-Brahmin servants and musicians in subordinate roles. The institutional complex is staffed and directed by Brahmin priests; it enforces the reading through spatial control (restricting temple entry), textual control (limiting who can read Vedas), and economic control (directing ritual income to Brahmin hands).
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, temple_institutional_complex, agenda_setter,
    institutional, civilizational, constrained, continental).

% The contested kernel itself — the body of Vedic texts whose interpretation grounds authority claims. The texts do not speak in a single voice on caste hierarchy or ritual exclusion. The hereditary-monopoly reading selects certain passages (Purusha Sukta hymn describing caste emergence, dharmaśāstra texts codifying ritual restrictions) and asserts they are definitive. Alternative readings select different passages (hymns addressing women, passages describing ritual participation across varnas, bhakti texts asserting sincere devotion) and assert those are definitive. The texts are the object all readings claim to interpret; they do not enforce a reading themselves — institutional power does.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, vedic_textual_tradition, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(vedic_dharmic_corpus__hereditary_monopoly_reading, vedic_textual_tradition).

% Historically mounted challenges to the hereditary monopoly reading through bhakti movements, vernacular devotional literature, and assertions that sincere devotion to the divine supersedes caste restrictions. Would have substantial objections to the reading if present in the interpretive conversation — but are historically marginalized from orthodox Brahmin centers of authority. Excluded from temple control, textual canonization, and the institutional legitimacy needed to reframe the corpus for society as a whole.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, devotional_reform_movements, excluded,
    organized, biographical, constrained, continental).

% 19th- and 20th-century activists and intellectuals (within and outside Hinduism) challenged the caste reading as historically constructed rather than divinely mandated, asserted that constitutional equality supersedes traditional hierarchy, and demanded vernacular access to texts and egalitarian ritual. Would fundamentally contest the hereditary monopoly reading if present in orthodox interpretive circles — but are excluded from Brahmin-controlled institutional authority and face suppression (texts are not translated into vernacular, alternative readings are labeled heretical or non-Hindu, institutional pressure discourages engagement).
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, egalitarian_reform_activists, excluded,
    organized, biographical, constrained, continental).

% Women scholars and practitioners who seek to reclaim interpretive authority and ritual participation in the Vedic tradition. Would contest the gendered exclusion enforced by the monopoly reading if given institutional platform. Excluded from traditional centers of Vedic authority, barred from studying in orthodox gurukula (schools), and face skepticism when asserting interpretive authority — yet increasingly claim seats through modern education and institutional challenge.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, independent_women_scholars, excluded,
    moderate, biographical, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_dharmic_corpus__hereditary_monopoly_reading, brahmin_priestly_class).
narrative_ontology:fixing_cost_class(vedic_dharmic_corpus__hereditary_monopoly_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint solves the problem: who conducts rituals that the Hindu religious cosmology asserts are necessary for human welfare and cosmic order? Who maintains temples and lineage traditions? Who interprets sacred texts when meaning is ambiguous? The hereditary monopoly reading asserts that Brahmins are the natural/ordained solvers of these problems because they are by birth the varna consecrated for intellectual and priestly labor. The coordination function is real: rituals are conducted, temples are maintained, textual traditions are transmitted, and communities have ritual specialists they can rely on.
% TRANSFER_FUNCTION: Moves ritual fees (cash payments for conducting ceremonies), land control (temple lands and donations), prestige (the title 'learned Brahmin' carries high social status), and interpretive authority (the power to define what the texts mean, which frames what counts as legitimate action and belief) from lower castes and women to the Brahmin priestly class. Lower castes and women must pay Brahmins to conduct their rituals; they cannot claim ritual authority; they cannot interpret texts with institutional force; they defer to Brahmin knowledge as authoritative. Brahmins collect the economic rents and monopolize the prestige.
% ABSENT_VOICES: Devotional reform movements (bhakti practitioners who assert sincere devotion supersedes caste) are marginalized from orthodox interpretive centers. Egalitarian reformers and women scholars are structurally excluded from institutions that control textual authority and ritual space. Lower-caste reformers and intellectuals who challenge the reading as historically constructed are delegitimized as heterodox. If these groups were seated in the interpretive conversation with equal authority, they would mount fundamental objections: that the texts do not mandate the hierarchy, that exclusions are unjust regardless of origin, that sincere devotion and rational critique are valid interpretive methods.
% DISAPPEARANCE_RATIONALE: If the hereditary monopoly reading and its institutional enforcement vanished overnight, the religious and social economy would substantially reorganize: lower castes would claim ritual roles previously denied, women would seek interpretive authority, alternative readings (bhakti, egalitarian) would surface as live options without institutional suppression, temple control would disperse or be contested, ritual fees would no longer concentrate in Brahmin hands, and the textual tradition would be subject to multiple competing interpretations rather than Brahmin monopoly. The constraint is not a discovery of natural fact but an institutional arrangement maintaining a particular distribution of authority and income. Its disappearance would leave the Vedic tradition intact (the texts would remain), but the power to interpret and benefit from them would be redistributed.
% FOUNDING_PROBLEM: Early Vedic ritual economy was complex and knowledge-intensive. Sacrifices, life-cycle rituals, and cosmic maintenance required specialists who understood the details of ritual action, the meanings of mantras, and the cosmology that situated the rituals as necessary. The founding problem: how to ensure rituals are performed correctly and that legitimate specialists are reliably available. The hereditary monopoly reading proposes: birth into Brahmin lineage ensures specialization (children learn from fathers), ensures reliability (the caste obligation binds them to the role), and ensures correctness (the varna is ordained for intellectual precision and ritual responsibility). The problem is real; this is one proposed solution among others.
% FOUNDING_PROBLEM_CORROBORATION: Brahmin institutional authorities attest the founding problem is still live — rituals are still complex, specialists are still needed, the lineage system still produces reliable practitioners. Reform movements and legal authorities attest the problem has been substantially solved differently: by written texts (anyone can learn ritual procedure), by modern professions (people can specialize without hereditary obligation), by democratic access (ritual knowledge should not be monopolized). Constitutional courts and egalitarian activists from outside the benefiting Brahmin community attest that the specialization and reliability could be achieved through open apprenticeship and education, without hereditary restriction. No corroborating voice from outside the Brahmin institutional framework supports the reading that specialization *requires* birth-based inheritance or that the hierarchy is the only workable solution.
narrative_ontology:disappearance_verdict(vedic_dharmic_corpus__hereditary_monopoly_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_dharmic_corpus__hereditary_monopoly_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_dharmic_corpus__hereditary_monopoly_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vedic_dharmic_corpus__hereditary_monopoly_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.65, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.65) because the constraint transfers substantial economic resources (ritual fees, land control, prestige) from lower-caste service providers and laity to Brahmin priests, and because the interpretive monopoly allows Brahmins to define what counts as legitimate knowledge and spiritual authority. The transfer is asymmetric: lower castes cannot exit the religious economy (attachment_coordination dependency, identity-locked birth) while Brahmins capture the gains. Suppression is correspondingly high (0.72) because the constraint depends on active institutional enforcement: temples police who may enter and perform ritual, initiation rites bar non-Brahmins, textual transmission is restricted to lineage members, and alternative readings are actively delegitimized. Theater is moderate (0.42) because while the reading performs 'discovery' of cosmic law, a substantial share of institutional activity is devoted to preventing exit (suppressing vernacular translation, blocking women's Vedic study, enforcing ritual restrictions through social sanction). The measurement series shows gradual intensification over the historical interval (extractiveness rising 0.58→0.65, suppression rising 0.68→0.72) as the reading became more institutionalized and more explicitly defended against challenge — not drift toward natural law, but intensifying enforcement of an institutional arrangement. The series shares one time grid (every metric authored at every time point 0, 200, 400, 600) so temporal analysis can integrate without interpolation.
 *
 * PERSPECTIVAL GAP:
 *   From the Brahmin priestly seat, the constraint is experienced as custodianship of a divinely-ordered system: ritual authority is a burden and responsibility, the hierarchy is cosmic necessity, and lower-caste compliance is natural alignment with truth. From the lower-caste seat, it is experienced as coercive exclusion: ritual performance is work they are barred from doing, the hierarchy is a cover story for domination, and compliance is enforced by spiritual threats (karma, rebirth as lower life forms) and material exclusion (denied ritual services, social stigma). The temple institutional complex experiences it as coordination infrastructure (temples are real community centers), but that coordination is layered atop extraction (the infrastructure is controlled by Brahmins, the income flows to them, access is restricted by birth status). The engine computes these divergent directionalities from the base structural data: beneficiary vs. victim, identity-locked vs. mobile, institutional vs. powerless. The claim (tangled_rope) reflects the ambiguity itself — is this primarily a coordination mechanism that happens to be unequal, or primarily an extraction mechanism defended by claims of cosmic order? The metrics (high extractiveness, high suppression) tilt toward extraction; the commentary documents why the coordination claim is not implausible.
 *
 * DIRECTIONALITY LOGIC:
 *   Brahmin priestly class sits at d near the beneficiary pole: they are the exclusive collectors of ritual fees and interpretive authority, they set the rules (agenda_setter role), they benefit from the reading's operation, and they have exit options (they can migrate, redefine themselves, practice alternative readings if they choose — though the reading makes such choices costly in status). Lower castes sit at d near the target pole: they pay ritual fees, they bear the extraction, they have no say in rule-setting, and they face identity-locked exit (birth defines their ritual status; exiting caste identity requires spiritual rebirth narratives outside this reading). Women of all castes sit in the target position regardless of class: even Brahmin women are excluded from full ritual authority and Vedic recitation by their gender, making them a split stakeholder group (Brahmin women benefit materially from Brahmin privilege but are suppressed within the ritual hierarchy). The beneficiary/victim asymmetry is sharp and forms the central structural fact of the tangled_rope gate: there is real coordination (the constraint solves the problem of who conducts life-cycle rituals, maintains temples, interprets law — genuine coordination work), but the coordination is structured to extract from a powerless, identity-locked majority in favor of a powerful minority who set the terms.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — 'who conducts rituals and maintains temples, what makes ritual action legitimate' — is still live from the Brahmin institutional perspective (temples are active, rituals are performed, the reading is still asserted). But the founding problem is contested: lower-caste reform movements, women's movements, constitutional law, and vernacular devotional traditions all assert that the problem was solved differently (anyone can conduct prayer, sincere devotion is sufficient, legal equality supersedes birth status). The constraint persists despite the founding problem being contested because Brahmin institutional authority has sufficient enforcement capacity (temple control, ritual monopoly, marriage/death-rite dependency) to maintain the reading even against legal challenge. The measurement trajectory shows suppression *increasing* (0.68→0.72) over the interval, not decreasing — institutional enforcement hardened against challenge rather than weakened. This is the opposite of mandatrophy resolution (where a constraint persists because enforcement decays and the institution stops defending the founding problem). The constraint shows *defense* against mandatrophy: Brahmins are doubling down on enforcement as the founding problem becomes more contested, not accepting the problem's obsolescence. Therefore mandatrophy is NOT resolved; the constraint is actively being maintained against the challenge that the founding problem is dead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_essentialism_vs_invention,
    'Is the varna hierarchy a logically necessary implication of the Vedic texts, or a constructed reading that privileges certain passages over others and ignores contradictory textual strands?',
    'Comparative textual analysis from outside the benefiting Brahmin interpretive tradition, specifically: examination of Vedic passages that describe ritual participation across varnas, passages that mandate ritual exclusion based on birth, and the historical dating of their composition; study of when the hierarchical reading became institutionalized vs. when it was contested within the tradition itself.',
    'If the hierarchy is textually necessary, the hereditary monopoly reading is a natural-law constraint (mountain). If it is a constructed reading, the constraint reclassifies as tangled_rope or snare depending on the enforcement mechanism''s dependence on suppression vs. active coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_essentialism_vs_invention, empirical, 'Whether varna hierarchy is an essential feature of the Vedic texts or a constructed interpretation.').

omega_variable(
    divine_ordination_claim_grounding,
    'What makes a claim ''divinely ordained'' in this reading''s epistemic framework? Is divinity grounded in the text itself, in Brahmin interpretive authority over the text, or in both circularly?',
    'Examination of the authority chain: (1) who claims the texts are divine, (2) who is authorized to read/interpret them, (3) whether a reading''s claim to divine ordination depends on being asserted by an authorized interpreter or on textual evidence independent of the interpreter''s seat.',
    'If divinity is grounded in the interpreter''s authority to declare it, the constraint is an extraction mechanism defended by circular legitimation (Brahmin authority grounds the divine claim; the divine claim grounds Brahmin authority). If divinity is independently textual, the circular inference is broken — but then it becomes falsifiable by alternative readings of the same texts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_ordination_claim_grounding, conceptual, 'The epistemic grounding of ''divine ordination'' claims in this reading.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.72) primarily structural (economic exclusion from ritual income, legal bars to interpretation, geographic temple barriers) or internalized (lower-caste agents have incorporated the reading into their self-concept and believe the hierarchy is rightfully ordained)?',
    'Post-liberation suppression trajectory: historical analysis of suppression dynamics in regions and time periods where the institutional machinery weakened (temples desecrated, texts translated into vernacular, colonial law challenged Brahmin authority). If suppression persists among lower-caste agents even after structural barriers fall, reclassify as substantially internalized.',
    'If structural, the constraint''s effective suppression is the measured 0.72; the target population could exit or resist if barriers fell. If internalized, the effective suppression is higher — the target carries the suppression with them after exit, limiting alternative framings available to them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression in this constraint is structural or internalized.').

omega_variable(
    kernel_reading_alternative_framings,
    'This constraint is ONE reading of the vedic_dharmic_corpus kernel. Do the sibling readings (bhakti_devotional_reading, reformist_egalitarian_reading) represent genuinely alternative interpretations within a single authoritative framework, or do they represent different commitments that cannot coexist within one framework?',
    'Examination of historical precedent: Can a single Brahmin interpreter or school hold both the hereditary monopoly reading (ritual authority from birth) and the bhakti reading (sincere devotion bypasses caste) simultaneously? Can both be asserted as compatible readings of the same Vedic texts? Or does asserting one rule out the other within the interpretive tradition''s own rules of inference?',
    'If readings genuinely coexist within the tradition, the relation is coexists_with (rivals within one framework). If one reading''s core premise logically forecloses the other, the relation is forecloses (incompatible within one framework). This omega documents the irreducible uncertainty about the reading-relation type.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_alternative_framings, conceptual, 'Whether sibling kernel readings coexist or foreclose one another within the interpretive tradition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_dharmic_corpus__hereditary_monopoly_reading, 0, 600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t0, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(vedi_tr_t200, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 200, 0.37).
narrative_ontology:measurement(vedi_tr_t400, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 400, 0.4).
narrative_ontology:measurement(vedi_tr_t600, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 600, 0.42).

% Extraction over time
narrative_ontology:measurement(vedi_be_t0, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(vedi_be_t200, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 200, 0.62).
narrative_ontology:measurement(vedi_be_t400, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 400, 0.64).
narrative_ontology:measurement(vedi_be_t600, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 600, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t0, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(vedi_su_t200, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 200, 0.69).
narrative_ontology:measurement(vedi_su_t400, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 400, 0.71).
narrative_ontology:measurement(vedi_su_t600, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 600, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_dharmic_corpus__hereditary_monopoly_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.12).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__hereditary_monopoly_reading, vedic_dharmic_corpus__bhakti_devotional_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__hereditary_monopoly_reading, vedic_dharmic_corpus__reformist_egalitarian_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of a constraint family decomposing the contested kernel vedic_dharmic_corpus into three structurally distinct readings. The hereditary-monopoly reading (this story) asserts high extractiveness (epsilon 0.65) with institutional enforcement via temple control and interpretive monopoly. The bhakti reading asserts lower extractiveness by claiming direct devotional access bypasses hierarchy. The reformist reading asserts the hierarchy is historically constructed, not textually essential. Each reading has different epsilon, different beneficiary/victim sets, different authority-grounding claims. They are linked via network.affects_constraints because each reading's success is structured pressure on the others — if bhakti gains institutional recognition, it undercuts the hereditary monopoly reading's claim to exclusivity; if reformist reading becomes state law, it changes the institutional conditions the monopoly reading operates in. The three readings are not observed truth competing over a fixed constraint; they are three different institutional readings of the same ambiguous textual kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vedic_dharmic_corpus__hereditary_monopoly_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
